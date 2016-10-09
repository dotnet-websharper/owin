namespace WebSharper.Owin

open System
open System.Collections.Generic
open System.IO
open System.Configuration
open System.Security.Principal
open System.Threading.Tasks
open System.Web
open System.Web.Security
open global.Owin
open Arachne.Http
open Arachne.Http.State
open Arachne.Uri
open WebSharper
open WebSharper.Sitelets
open WebSharper.Web
module Rem = WebSharper.Core.Remoting
module Res = WebSharper.Core.Resources
module P = WebSharper.PathConventions
module M = WebSharper.Core.Metadata

#if ZAFIR
type DepG = WebSharper.Core.DependencyGraph.Graph
#endif

type Uri = System.Uri

type Env = IDictionary<string, obj>
type HeaderDictionary = IDictionary<string, string[]>
type AppFunc = Func<Env, Task>
type MidFunc = Func<AppFunc, AppFunc>

// TODO: Use Arachne or Freya.Core instead.
module private Environment =
    let getHost (env : Env) =
        let headers : HeaderDictionary = unbox env.["owin.RequestHeaders"]
        let host =
            if headers.ContainsKey("Host") then
                unbox headers.["Host"].[0]
            else ""
        match host with
        | null | "" ->
            let localIpAddress =
                if env.ContainsKey("server.LocalIpAddress") then
                    match unbox env.["server.LocalIpAddress"] with
                    | null | "" -> "localhost"
                    | localIp -> localIp
                else "localhost"
            let localPort =
                if env.ContainsKey("server.LocalPort") then
                    unbox env.["server.LocalPort"]
                else ""
            if String.IsNullOrWhiteSpace localPort then localIpAddress else localIpAddress + ":" + localPort
        | _ -> host

    let getBaseUri (env : Env) =
        unbox env.["owin.RequestScheme"] + "://" +
        getHost env +
        if String.IsNullOrEmpty (unbox env.["owin.RequestPathBase"]) then "/" else unbox env.["owin.RequestPathBase"]

    let getRequestUri (env : Env) =
        unbox env.["owin.RequestScheme"] + "://" +
        getHost env +
        (unbox env.["owin.RequestPathBase"]) +
        (unbox env.["owin.RequestPath"]) +
        if String.IsNullOrEmpty (unbox env.["owin.RequestQueryString"]) then "" else "?" + (unbox env.["owin.RequestQueryString"])

    let appendHeader (key : string, value : string) (headers : HeaderDictionary) =
        headers.[key] <-
            if headers.ContainsKey(key) then
                Array.append headers.[key] [|value|]
            else [|value|]

[<NoComparison; NoEquality>]
type Options =
    internal {
        Debug : bool
        JsonProvider : Core.Json.Provider
        Metadata : M.Info
        Dependencies : DepG
        ServerRootDirectory : string
        UrlPrefix : string
        RemotingServer : option<Rem.Server>
        OnException : bool -> Env -> exn -> Task
    }

    member o.WithDebug() = o.WithDebug(true)
    member o.WithDebug(d) = { o with Debug = d }
    member o.WithServerRootDirectory(d) = { o with ServerRootDirectory = d }
    member o.WithUrlPrefix(t) = { o with UrlPrefix = t }
    member o.WithOnException(f) = { o with OnException = f }

    static member DefaultOnException (debug: bool) (env: Env) (e: exn) =
        env.["owin.ResponseStatusCode"] <- 500
        env.["owin.ResponseReasonPhrase"] <- "Internal Server Error"
        let bytes = Text.Encoding.UTF8.GetBytes(if debug then sprintf "%A" e else "Internal Server Error")
        (env.["owin.ResponseBody"] :?> Stream).WriteAsync(bytes, 0, bytes.Length)

    static member Create() =
        let dir = System.IO.Directory.GetCurrentDirectory()
        {
            Debug = false
            JsonProvider = Core.Json.Provider.Create()
#if ZAFIR
            Metadata = M.Info.Empty
            Dependencies = DepG.Empty
#else
            Metadata = M.Info.Create([])
#endif
            ServerRootDirectory = dir
            UrlPrefix = ""
            RemotingServer = None
            OnException = Options.DefaultOnException
        }

[<AutoOpen>]
module private Internal =

    open System.Reflection
    open HttpMultipartParser

    let [<Literal>] OwinContextKey = "OwinContext"
    let [<Literal>] HttpContextKey = "HttpContext"
    let [<Literal>] WebSharperRequestKey = "WebSharper.Request"

    [<NoComparison; NoEquality>]
    type FormData =
        {
            Files : seq<HttpPostedFileBase>
            Fields : Http.ParameterCollection
            Body : Stream
        }

    module Seq =
        let tryHead (seq: seq<'t>) =
            let e = seq.GetEnumerator()
            if e.MoveNext() then Some e.Current else None

    module O2W =

        let Method (m: string) : Http.Method =
            match m.ToLower() with
            | "connect" -> Http.Method.Connect
            | "delete" -> Http.Method.Delete
            | "get" -> Http.Method.Get
            | "head" -> Http.Method.Head
            | "options" -> Http.Method.Options
            | "post" -> Http.Method.Post
            | "put" -> Http.Method.Put
            | "trace" -> Http.Method.Trace
            | s -> Http.Method.Custom s

        let Headers (headers: HeaderDictionary) : seq<Http.Header> =
            seq {
                for KeyValue(k, vs) in headers do
                    for v in vs do
                        yield Http.Header.Custom k v
            }

        let QueryParams (uri: Uri) : Http.ParameterCollection =
            let query = Query.parse uri.Query
            match fst Query.pairs_ query with
            | Some qs ->
                Http.ParameterCollection(
                    seq {
                        for k, v in qs do
                            yield k, match v with Some str -> str | None -> null
                    }
                )
            | None -> Http.ParameterCollection(Seq.empty)

        let tryFindCookieHeader (headers : HeaderDictionary) =
            if headers.ContainsKey("Cookie") then
                headers.["Cookie"]
                |> Array.map Cookie.parse
                |> Some
            else None

        let Cookies (cookies : Cookie[] option) : HttpCookieCollection =
            let coll = HttpCookieCollection()
            match cookies with
            | Some cookies ->
                for (Cookie cookie) in cookies do
                    for Pair(State.Name k, Value v) in cookie do
                        coll.Add(HttpCookie(k, v))
                coll
            | None -> coll

        let DefaultCharset = System.Text.Encoding.GetEncoding("ISO-8859-1")
        let re = System.Text.RegularExpressions.Regex("; *charset *= *([^;]+)")
        let (|Multipart|UrlEnc|) (req: Env) =
            let headers : HeaderDictionary = unbox req.["owin.RequestHeaders"]
            match headers.TryGetValue "Content-Type" with
            | true, ctypes when not (Array.isEmpty ctypes) ->
                let ctype = ctypes.[0].ToLower()
                if ctype.StartsWith "multipart/form-data" then
                    Multipart
                else
                    let m = re.Match ctype
                    if m.Success then
                        UrlEnc (System.Text.Encoding.GetEncoding m.Groups.[1].Value)
                    else UrlEnc DefaultCharset
            | _ -> UrlEnc DefaultCharset

        let ParseFormData (req: Env) =
            let body = new MemoryStream()
            (req.["owin.RequestBody"] :?> Stream).CopyTo body
            body.Seek(0L, SeekOrigin.Begin) |> ignore
            match req with
            | Multipart ->
                let parser = new MultipartFormDataParser(body)
                let fields = [| for KeyValue(k, v) in parser.Parameters -> k, v.Data |]
                let files =
                    [|
                        for f in parser.Files ->
                            let length = int f.Data.Length
                            { new HttpPostedFileBase() with
                                member this.ContentLength = length
                                member this.ContentType = f.ContentType
                                member this.FileName = f.FileName
                                member this.InputStream = f.Data
                                member this.SaveAs(filename) =
                                    if f.Data.CanSeek then
                                        f.Data.Seek(0L, SeekOrigin.Begin) |> ignore
                                    use w = File.OpenWrite(filename)
                                    f.Data.CopyTo w }
                    |]
                { Files = files; Fields = Http.ParameterCollection(fields); Body = body }
            | UrlEnc enc ->
                use s = new StreamReader(body, enc, false, 1024, true)
                let q = System.Web.HttpUtility.ParseQueryString(s.ReadToEnd())
                { Files = []; Fields = Http.ParameterCollection(q); Body = body }

        let private request (env: Env) : Http.Request =
            let formData = ParseFormData env
            formData.Body.Seek(0L, SeekOrigin.Begin) |> ignore
            let uri =
                let requestUri = Uri(Environment.getRequestUri env)
                match unbox env.["owin.RequestPathBase"] with
                | "" | "/" -> requestUri
                | pathBase ->
                    if requestUri.IsAbsoluteUri then
                        let uB = UriBuilder requestUri
                        if uB.Path.StartsWith pathBase then
                            uB.Path <- uB.Path.Substring pathBase.Length
                        uB.Uri
                    else
                        requestUri
            let headers : HeaderDictionary = unbox env.["owin.RequestHeaders"]
            {
                Method = Method (unbox env.["owin.RequestMethod"])
                Uri = uri
                Headers = Headers headers
                Post = formData.Fields
                Get = QueryParams uri
                Cookies = Cookies (tryFindCookieHeader headers)
                ServerVariables = Http.ParameterCollection([])
                Body = formData.Body
                Files = formData.Files
            }

        let Request (env: Env) : Http.Request =
            match env.TryGetValue WebSharperRequestKey with
            | true, (:? Http.Request as req) -> req
            | _ ->
                let req = request env
                env.[WebSharperRequestKey] <- req
                req

    module W2O =

        let WriteResponse (resp: Task<Http.Response>) (out: Env) (onException: Env -> exn -> Task) =
            resp.ContinueWith(fun (t: Task<Http.Response>) ->
                let body : Stream = unbox out.["owin.ResponseBody"]
                try
                    match t.Exception with
                    | null ->
                        let resp = t.Result
                        out.["owin.ResponseStatusCode"] <- resp.Status.Code
                        let headers : HeaderDictionary = unbox out.["owin.ResponseHeaders"]
                        for name, hs in resp.Headers |> Seq.groupBy (fun h -> h.Name) do
                            let existing =
                                if headers.ContainsKey(name) then
                                    headers.[name]
                                else [||]
                            let combined =
                                if Array.isEmpty existing then
                                    [| for h in hs -> h.Value |]
                                else Array.append existing [| for h in hs -> h.Value |]
                            headers.[name] <- combined
                        let str = new MemoryStream()
                        resp.WriteBody(str :> _)
                        let bytes = str.ToArray()
                        body.Write(bytes, 0, bytes.Length)
                    | e ->
                        (onException out e).Wait()
                with e ->
                    (onException out e).Wait()
            )

    let buildResourceContext cfg (context: Env) : Res.Context =
        let isDebug = cfg.Debug
        let pu = P.PathUtility.VirtualPaths(unbox context.["owin.RequestPathBase"])
        {
            DebuggingEnabled = isDebug
            DefaultToHttp = false
            GetSetting = fun (name: string) ->
                match ConfigurationManager.AppSettings.[name] with
                | null -> None
                | x -> Some x
            GetAssemblyRendering = fun name ->
#if ZAFIR
                let aid = P.AssemblyId.Create(name)
#else
                let aid = P.AssemblyId.Create(name.FullName)
#endif
                let url = if isDebug then pu.JavaScriptPath(aid) else pu.MinifiedJavaScriptPath(aid)
                Res.RenderLink url
            GetWebResourceRendering = fun ty resource ->
                let id = P.AssemblyId.Create(ty)
                let kind =
                    if resource.EndsWith(".js") || resource.EndsWith(".ts")
                        then P.ResourceKind.Script
                        else P.ResourceKind.Content
                P.EmbeddedResource.Create(kind, id, resource)
                |> pu.EmbeddedPath
                |> Res.RenderLink
            RenderingCache = System.Collections.Concurrent.ConcurrentDictionary()
            ResourceDependencyCache = System.Collections.Concurrent.ConcurrentDictionary()
        }

    // Store WebSharper user identity in the environment dictionary,
    // avoid overwriting principal set by OWIN authentication middleware
    let [<Literal>] WebSharperUserKey = "WebSharper.User"

    type OwinCookieUserSession(ctx: Env) =
        let requestHeaders : HeaderDictionary = unbox ctx.["owin.RequestHeaders"]
        let responseHeaders : HeaderDictionary = unbox ctx.["owin.ResponseHeaders"]

        let refresh (cookie: string) =
            match cookie with
            | null -> ctx.[WebSharperUserKey] <- None
            | cookie ->
                let ticket = FormsAuthentication.Decrypt cookie
                let principal = GenericPrincipal(FormsIdentity(ticket), [||])
                ctx.[WebSharperUserKey] <- Some principal
            |> ignore 

        let ensureUserHasBeenRefreshed () = 
            if ctx.ContainsKey(WebSharperUserKey) |> not then 
                // Using `try ... with` because `FormsAuthentication.Decrypt`
                // throws an exception when there is a cookie but its format is invalid
                try //refresh ctx.Request.Cookies.[FormsAuthentication.FormsCookieName]
                    match O2W.tryFindCookieHeader requestHeaders with
                    | Some cookies ->
                        let c =
                            cookies
                            |> List.ofArray
                            |> List.collect (fun (Cookie pairs) -> pairs)
                            |> List.tryFind (fun (Pair(State.Name k, _)) ->
                                k = FormsAuthentication.FormsCookieName)
                        match c with
                        | Some (Pair(_, Value v)) -> refresh v
                        | None -> refresh null
                    | None -> refresh null
                with _ -> refresh null

        interface IUserSession with

            member this.IsAvailable = true

            member this.GetLoggedInUser() =
                async {
                    ensureUserHasBeenRefreshed()
                    match unbox<GenericPrincipal option> ctx.[WebSharperUserKey] with
                    | None -> return None
                    | Some x ->
                        if x.Identity.IsAuthenticated then
                            return Some x.Identity.Name
                        else return None
                }

            member this.LoginUser(user, ?persistent) =
                async {
                    let persistent = defaultArg persistent false
                    let cookie = FormsAuthentication.GetAuthCookie(user, persistent)
                    let setCookie =
                        SetCookie(
                            Pair(State.Name cookie.Name, Value cookie.Value),
                            Attributes [
                                if not (String.IsNullOrEmpty cookie.Domain) then yield Domain (Domain.parse cookie.Domain)
                                if persistent then yield Expires cookie.Expires
                                if not (String.IsNullOrEmpty cookie.Path) then yield Path cookie.Path
                                if cookie.HttpOnly then yield HttpOnly
                                if cookie.Secure then yield Secure
                            ])
                    Environment.appendHeader ("Set-Cookie", SetCookie.format setCookie) responseHeaders
                    return refresh cookie.Value
                }

            member this.Logout() =
                async {
                    let setCookie =
                        SetCookie(
                            Pair(State.Name FormsAuthentication.FormsCookieName, Value ""),
                            Attributes [ Expires (DateTime.Now.AddDays(-1.)) ])
                    Environment.appendHeader ("Set-Cookie", SetCookie.format setCookie) responseHeaders
                    return refresh null
                }

    let mkEnv (owinContext: Env) (httpContext: HttpContext) =
        owinContext.[OwinContextKey] <- owinContext
        match httpContext with
        | null -> ()
        | x -> owinContext.[HttpContextKey] <- HttpContextWrapper(x)
        owinContext

    [<Sealed>]
    type ContextBuilder(cfg) =
        let info = cfg.Metadata
        let graph = cfg.Dependencies
        let json = cfg.JsonProvider
        let resContext = buildResourceContext cfg

        let ( ++ ) a b =
            let a =
                match a with
                | "" -> "/"
                | _ -> VirtualPathUtility.AppendTrailingSlash(a)
            let b =
                match b with
                | "" -> "."
                | _ -> b
            VirtualPathUtility.Combine(a, b)

        let resolveUrl context appPath u =
            if VirtualPathUtility.IsAppRelative(u) then
                VirtualPathUtility.ToAbsolute(u, appPath)
            else
                u

        member b.GetContext<'T when 'T : equality>(site: Sitelet<'T>, req: Http.Request, context: Env, httpContext: HttpContext) : Context<'T> =
            let appPath = unbox context.["owin.RequestPathBase"]
            let link = site.Router.Link
            let prefix = cfg.UrlPrefix
            let p = appPath ++ prefix
            let link x =
                match link x with
                | None -> failwithf "Failed to link to %O" (box x)
                | Some loc ->
                    if loc.IsAbsoluteUri then string loc else
                        let loc =
                            match string loc with
                            | "" | "/" -> "."
                            | s when s.StartsWith("/") -> s.Substring(1)
                            | s -> s
                        p ++ loc
            new Context<'T>(
                ApplicationPath = appPath,
                Environment = mkEnv context httpContext,
                Link = link,
                Json = json,
                Metadata = info,
                Dependencies = graph,
                ResolveUrl = resolveUrl context appPath,
                ResourceContext = resContext context,
                Request = req,
                RootFolder = cfg.ServerRootDirectory,
                UserSession = OwinCookieUserSession(context)
            )

    let dispatch (cb: ContextBuilder) (s: Sitelet<'T>) (context: Env) (httpContext: HttpContext) onException : option<Task> =
        try
            let request = O2W.Request context
            let ctx = cb.GetContext(s, request, context, httpContext)
            s.Router.Route(request)
            |> Option.map (fun action ->
                let content = s.Controller.Handle(action)
                let response = Content.ToResponse content ctx |> Async.StartAsTask
                W2O.WriteResponse response context onException)
        with e ->
            Some (onException context e)

    type Assembly =

        static member LoadFileInfo(info: FileInfo) =
            let name = AssemblyName.GetAssemblyName(info.FullName)
            match Assembly.TryLoad(name) with
            | None -> Assembly.LoadFrom(info.FullName)
            | Some a -> a

        static member TryLoad(name: AssemblyName) =
            try
                match Assembly.Load(name) with
                | null -> None
                | a -> Some a
            with _ -> None

    type DirectoryInfo with

        member dir.DiscoverAssemblies() =
            let ls pat = dir.EnumerateFiles(pat)
            let ( @ ) = Seq.append
            ls "*.dll" @ ls "*.exe"

    type M.Info with

        static member LoadFromBinDirectory(binDirectory: string) =
            let d = DirectoryInfo(binDirectory)
            let refs =
                d.DiscoverAssemblies()
                |> Seq.choose (fun f -> M.IO.LoadReflected(Assembly.LoadFileInfo f))
                |> List.ofSeq
            M.Info.UnionWithoutDependencies refs, DepG.FromData(refs |> Seq.map (fun m -> m.Dependencies))

        static member LoadFromWebRoot(webRoot: string) =
            M.Info.LoadFromBinDirectory(Path.Combine(webRoot, "bin"))

type Options with

    static member Create(meta, graph) =
        let dir = System.IO.Directory.GetCurrentDirectory()
#if ZAFIR
        let json = Core.Json.Provider.CreateTyped meta
        let remotingServer = Rem.Server.Create meta json
#else
        let remotingServer = Rem.Server.Create None meta
#endif
        {
            Debug = false
            JsonProvider = json
            Metadata = meta
            Dependencies = graph
            ServerRootDirectory = dir
            UrlPrefix = ""
            RemotingServer = Some remotingServer
            OnException = Options.DefaultOnException
        }

    member o.WithRunRemoting(b) =
        let server =
#if ZAFIR
            if b then Some (Rem.Server.Create o.Metadata o.JsonProvider) else None
#else
            if b then Some (Rem.Server.Create None o.Metadata) else None
#endif
        { o with RemotingServer = server }

    static member Create(webRoot, ?binDirectory) =
        let binDirectory = defaultArg binDirectory (Path.Combine(webRoot, "bin"))
        let meta, graph = M.Info.LoadFromBinDirectory(binDirectory)
        Options.Create(meta, graph)
            .WithServerRootDirectory(webRoot)

    member o.WithBinDirectory(dir) =
        let meta, graph = M.Info.LoadFromBinDirectory dir
        { o with Metadata = meta; Dependencies = graph }

type RemotingMiddleware(next: AppFunc, webRoot: string, server: Rem.Server, onException: Env -> exn -> Task) =

    member this.Invoke(env: Env) =
        let req = O2W.Request env
        let httpContext = HttpContext.Current
        let respBody : Stream = unbox env.["owin.ResponseBody"]
        let respHeaders = unbox<IDictionary<string, string[]>> env.["owin.ResponseHeaders"]
        let addHeader k v =
            respHeaders.[k] <-
                match respHeaders.TryGetValue k with
                | true, coll -> Array.append coll v
                | false, _ -> v
        let addHeaders headers =
            for k, vs in Seq.groupBy fst headers do
                addHeader k [| for _, v in vs -> v |]
        let getHeader k = req.Headers |> Seq.tryPick (fun h -> if h.Name = k then Some h.Value else None)
        if Rem.IsRemotingRequest getHeader then
            async {
                try
                    match WebSharper.Web.RpcHandler.CorsAndCsrfCheck (unbox env.["owin.RequestMethod"]) req.Uri
                            (fun k -> match req.Cookies.[k] with null -> None | x -> Some x.Value)
                            getHeader
                            (fun k v ->
                                let expires = System.DateTime.UtcNow.AddYears(1000).ToString("R")
                                addHeader "Set-Cookie" [| sprintf "%s=%s; Expires=%s" k v expires |])
                        with
                    | WebSharper.Web.Error (code, msg) ->
                        env.["owin.ResponseStatusCode"] <- code
                        env.["owin.ResponseReasonPhrase"] <- msg
                        use w = new StreamWriter(env.["owin.ResponseBody"] :?> Stream)
                        w.Write msg
                    | WebSharper.Web.Preflight headers ->
                        addHeaders headers
                    | WebSharper.Web.Ok headers ->
                        addHeaders headers
                        use reader = new StreamReader(req.Body)
                        let session = new OwinCookieUserSession(env)
                        let ctx =
                            { new Web.IContext with
                                member this.UserSession = session :> _
                                member this.RequestUri = req.Uri
                                member this.RootFolder = webRoot
                                member this.Environment = mkEnv env httpContext }
                        let! body = reader.ReadToEndAsync() |> Async.AwaitTask
                        let! resp =
                            server.HandleRequest(
                                {
                                    Body = body
                                    Headers = getHeader
                                }, ctx)
                        env.["owin.ResponseStatusCode"] <- box 200
                        let respHeaders : HeaderDictionary = unbox env.["owin.ResponseHeaders"]
                        respHeaders.["Content-Type"] <- [|resp.ContentType|]
                        let bytes = Text.Encoding.UTF8.GetBytes(resp.Content)
                        respBody.Write(bytes, 0, bytes.Length)
                with e ->
                    return! onException env e |> Async.AwaitIAsyncResult |> Async.Ignore
            }
            |> Async.StartAsTask
            :> Task
        else next.Invoke(env)

    // (options)

    new (next, options: Options) =
        new RemotingMiddleware(next, options.ServerRootDirectory, options.RemotingServer.Value,
            options.OnException options.Debug)

    static member AsMidFunc(options: Options) =
        MidFunc(fun next ->
            AppFunc(RemotingMiddleware(next, options).Invoke))

    // (webRoot, ?binDirectory)

    static member UseRemoting(webRoot: string, ?binDirectory: string) =
        let meta, graph =
            match binDirectory with
            | None -> M.Info.LoadFromWebRoot(webRoot)
            | Some binDirectory -> M.Info.LoadFromBinDirectory(binDirectory)
        let o = Options.Create(meta, graph).WithServerRootDirectory(webRoot)
        fun next -> new RemotingMiddleware(next, o)

    static member AsMidFunc(webRoot: string, ?binDirectory: string) =
        let mw = RemotingMiddleware.UseRemoting(webRoot, ?binDirectory = binDirectory)
        MidFunc(fun next -> AppFunc(mw(next).Invoke))

type SiteletMiddleware<'T when 'T : equality>(next: AppFunc, config: Options, sitelet: Sitelet<'T>) =
    let cb = ContextBuilder(config)

    let appFunc =
        let siteletAppFunc = AppFunc(fun env ->
            let httpContext = HttpContext.Current // non-null if running on top of asp.net
            match dispatch cb sitelet env httpContext (config.OnException config.Debug) with
            | Some t -> t
            | None -> next.Invoke(env))
        if config.RemotingServer.IsSome then
            AppFunc(RemotingMiddleware(siteletAppFunc, config).Invoke)
        else
            siteletAppFunc

    member this.Invoke(env: Env) =
        appFunc.Invoke(env)

    static member AsMidFunc(config: Options, sitelet: Sitelet<'T>) =
        MidFunc(fun next ->
            AppFunc(SiteletMiddleware(next, config, sitelet).Invoke))

    static member UseDiscoveredSitelet(webRoot: string, ?binDirectory: string) =
        let binDirectory = defaultArg binDirectory (Path.Combine(webRoot, "bin"))
        let binDir = DirectoryInfo(binDirectory)
        let ok =
            try
                binDir.DiscoverAssemblies()
                |> Seq.choose (fun p ->
                    try Some (Assembly.LoadFileInfo(p))
                    with e -> None)
                |> HttpModule.DiscoverSitelet
                |> Option.map (fun sitelet ->
                    let options = Options.Create(webRoot, binDirectory = binDirectory)
                    fun next -> SiteletMiddleware<obj>(next, options, sitelet))
            with :? System.Reflection.ReflectionTypeLoadException as exn ->
                failwithf "%A" (exn.LoaderExceptions)
        match ok with
        | Some this -> this
        | None -> failwith "Failed to discover sitelet assemblies"

    static member UseDiscoveredSitelet(config: Options, binDirectory: string) =
        let binDir = DirectoryInfo(binDirectory)
        let ok =
            try
                binDir.DiscoverAssemblies()
                |> Seq.choose (fun p ->
                    try Some (Assembly.LoadFileInfo(p))
                    with e -> None)
                |> HttpModule.DiscoverSitelet
                |> Option.map (fun sitelet ->
                    fun next -> SiteletMiddleware<obj>(next, config, sitelet))
            with :? System.Reflection.ReflectionTypeLoadException as exn ->
                failwithf "%A" (exn.LoaderExceptions)
        match ok with
        | Some this -> this
        | None -> failwith "Failed to discover sitelet assemblies"

    static member AsMidFunc(webRoot: string, ?binDirectory: string) =
        let mw = SiteletMiddleware<obj>.UseDiscoveredSitelet(webRoot, ?binDirectory = binDirectory)
        MidFunc(fun next -> AppFunc(mw(next).Invoke))

type InitAction = Owin.IAppBuilder * WebSharper.Core.Json.Provider * (Env -> Web.IContext) -> unit

type WebSharperOptions<'T when 'T : equality>() = 
    let mutable binDir = None
    let mutable initActions = []

    member val ServerRootDirectory = System.IO.Directory.GetCurrentDirectory() with get, set
    member this.BinDirectory
        with get () = 
            match binDir with
            | None -> System.IO.Path.Combine(this.ServerRootDirectory, "bin")
            | Some d -> d
        and set dir = binDir <- Some dir

    member val UseRemoting = true with get, set
    member val UrlPrefix = "" with get, set
    member val Debug = false with get, set
    member val Sitelet = None with get, set
    member val DiscoverSitelet = false with get, set
    member val MetadataAndGraph = None with get, set
    member val OnException = Options.DefaultOnException with get, set

    static member DefaultOnException debug response exn =
        Options.DefaultOnException debug response exn
    
    member internal this.InitActions = initActions

    member this.WithSitelet(sitelet: Sitelet<'T>) =
        this.Sitelet <- Some sitelet 
        this

    member this.WithInitAction(action) = 
        initActions <- action :: initActions   
        this  

    member internal this.Run(builder: IAppBuilder) =
        let meta, graph = 
            match this.MetadataAndGraph with
            | Some m -> m
            | None ->
                if this.UseRemoting || Option.isSome this.Sitelet || this.DiscoverSitelet then
                    M.Info.LoadFromBinDirectory(this.BinDirectory)
                else
#if ZAFIR
                    M.Info.Empty, DepG.Empty
#else
                    M.Info.Create([])
#endif
             
        let remotingServer, jsonProvider =
            if this.UseRemoting then
#if ZAFIR
                let json = Core.Json.Provider.CreateTyped meta
                let rem = Rem.Server.Create meta json
#else
                let rem = Rem.Server.Create None meta
#endif
                Some rem, rem.JsonProvider
            else None, Core.Json.Provider.Create()

        let config = {
            Debug = this.Debug
            JsonProvider = jsonProvider
            Metadata = meta
            Dependencies = graph
            ServerRootDirectory = this.ServerRootDirectory
            UrlPrefix = this.UrlPrefix
            RemotingServer = remotingServer
            OnException = this.OnException
        }

        match this.Sitelet with
        | Some sitelet -> builder.Use(SiteletMiddleware<'T>.AsMidFunc(config, sitelet)) |> ignore
        | _ ->
            if this.DiscoverSitelet then
                let mw = SiteletMiddleware<obj>.UseDiscoveredSitelet(config, this.BinDirectory)
                builder.Use(MidFunc(fun next -> AppFunc(mw(next).Invoke))) |> ignore
            elif this.UseRemoting then
                builder.Use(RemotingMiddleware.AsMidFunc(config)) |> ignore    

        if not (List.isEmpty this.InitActions) then
            let mkCtx (context: Env) =
                let env = mkEnv context HttpContext.Current
                let req = env.[WebSharperRequestKey] :?> Http.Request
                let session = new OwinCookieUserSession(context)
                { new IContext with
                    member ctx.Environment = env
                    member ctx.RequestUri = req.Uri
                    member ctx.RootFolder = this.ServerRootDirectory
                    member ctx.UserSession = session :> _
                }
            for a in this.InitActions do
                a(builder, jsonProvider, mkCtx)

[<AutoOpen>]
module Extensions =
    type IAppBuilder with

        member this.UseWebSharperRemoting(webRoot: string, meta: M.Info) =
            this.Use(RemotingMiddleware.AsMidFunc(Options.Create(meta, DepG.FromData([ meta.Dependencies ])).WithServerRootDirectory(webRoot)))

        member this.UseWebSharperRemoting(meta: M.Info) =
            this.Use(RemotingMiddleware.AsMidFunc(Options.Create(meta, DepG.FromData([ meta.Dependencies ]))))

        member this.UseWebSharperRemoting(webRoot: string, ?binDirectory: string) =
            this.Use(RemotingMiddleware.AsMidFunc(webRoot, ?binDirectory = binDirectory))

        member this.UseWebSharperRemotingFromBin(binDirectory: string) =
            this.Use(RemotingMiddleware.AsMidFunc(binDirectory, binDirectory = binDirectory))

        member this.UseSitelet(webRoot: string, sitelet, ?binDirectory) =
            this.UseCustomSitelet(Options.Create(webRoot, ?binDirectory = binDirectory), sitelet)

        member this.UseCustomSitelet(config: Options, sitelet: Sitelet<'T>) =
            this.Use(SiteletMiddleware<'T>.AsMidFunc(config, sitelet))

        member this.UseDiscoveredSitelet(webRoot: string, ?binDirectory) =
            this.Use(SiteletMiddleware<obj>.AsMidFunc(webRoot, ?binDirectory = binDirectory))

        member this.UseWebSharper(options: WebSharperOptions<'T>) =
            options.Run(this)
            this
