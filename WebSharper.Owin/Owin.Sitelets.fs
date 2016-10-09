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

type DepG = WebSharper.Core.DependencyGraph.Graph
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
            Metadata = M.Info.Empty
            Dependencies = DepG.Empty
            ServerRootDirectory = dir
            UrlPrefix = ""
            RemotingServer = None
            OnException = Options.DefaultOnException
        }

    static member DefaultBinDirectory =
        Path.GetDirectoryName(typeof<Options>.Assembly.Location)

module EnvKey =
    let HttpContext = "HttpContext"
    module WebSharper =
        let Request = "WebSharper.Request"
        let Context = "WebSharper.Context"
        // Store WebSharper user identity in the environment dictionary,
        // avoid overwriting principal set by OWIN authentication middleware
        let User = "WebSharper.User"

    let internal GetOrSet<'T> (env: Env) (key: string) (mk: Env -> 'T) =
        match env.TryGetValue key with
        | true, (:? 'T as x) -> x
        | _ ->
            let x = mk env
            env.[key] <- x
            x

[<AutoOpen>]
module private Internal =

    open System.Reflection
    open HttpMultipartParser

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

    let tryFindCookieHeader (headers : HeaderDictionary) =
        if headers.ContainsKey("Cookie") then
            headers.["Cookie"]
            |> Array.map Cookie.parse
            |> Some
        else None

    type OwinCookieUserSession(ctx: Env) =
        let requestHeaders : HeaderDictionary = unbox ctx.["owin.RequestHeaders"]
        let responseHeaders : HeaderDictionary = unbox ctx.["owin.ResponseHeaders"]

        let refresh (cookie: string) =
            match cookie with
            | null -> ctx.[EnvKey.WebSharper.User] <- None
            | cookie ->
                let ticket = FormsAuthentication.Decrypt cookie
                let principal = GenericPrincipal(FormsIdentity(ticket), [||])
                ctx.[EnvKey.WebSharper.User] <- Some principal
            |> ignore 

        let ensureUserHasBeenRefreshed () = 
            if ctx.ContainsKey(EnvKey.WebSharper.User) |> not then 
                // Using `try ... with` because `FormsAuthentication.Decrypt`
                // throws an exception when there is a cookie but its format is invalid
                try //refresh ctx.Request.Cookies.[FormsAuthentication.FormsCookieName]
                    match tryFindCookieHeader requestHeaders with
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
                    match unbox<GenericPrincipal option> ctx.[EnvKey.WebSharper.User] with
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
            EnvKey.GetOrSet<Http.Request> env EnvKey.WebSharper.Request request

        let SetHttpContext (env: Env) : unit =
            match HttpContext.Current with
            | null -> ()
            | x -> env.[EnvKey.HttpContext] <- HttpContextWrapper(x)

        let SimpleContext rootDir (env: Env) : Web.IContext =
            EnvKey.GetOrSet<Web.IContext> env EnvKey.WebSharper.Context <| fun env ->
                SetHttpContext env
                let req = Request env
                let session = lazy new OwinCookieUserSession(env)
                { new IContext with
                    member ctx.Environment = env
                    member ctx.RequestUri = req.Uri
                    member ctx.RootFolder = rootDir
                    member ctx.UserSession = session.Value :> _
                }

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

    let buildResourceContext cfg (env: Env) : Res.Context =
        let isDebug = cfg.Debug
        let pu = P.PathUtility.VirtualPaths(unbox env.["owin.RequestPathBase"])
        {
            DebuggingEnabled = isDebug
            DefaultToHttp = false
            GetSetting = fun (name: string) ->
                match ConfigurationManager.AppSettings.[name] with
                | null -> None
                | x -> Some x
            GetAssemblyRendering = fun name ->
                let aid = P.AssemblyId.Create(name)
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

        let resolveUrl appPath u =
            if VirtualPathUtility.IsAppRelative(u) then
                VirtualPathUtility.ToAbsolute(u, appPath)
            else
                u

        member b.GetContext<'T when 'T : equality>(site: Sitelet<'T>, req: Http.Request, env: Env) : Context<'T> =
            let appPath = unbox env.["owin.RequestPathBase"]
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
            EnvKey.GetOrSet<Context<'T>> env EnvKey.WebSharper.Context <| fun env ->
                O2W.SetHttpContext env
                new Context<'T>(
                    ApplicationPath = appPath,
                    Environment = env,
                    Link = link,
                    Json = json,
                    Metadata = info,
                    Dependencies = graph,
                    ResolveUrl = resolveUrl appPath,
                    ResourceContext = resContext env,
                    Request = req,
                    RootFolder = cfg.ServerRootDirectory,
                    UserSession = OwinCookieUserSession(env)
                )

    let dispatch (cb: ContextBuilder) (s: Sitelet<'T>) (env: Env) onException : option<Task> =
        try
            let request = O2W.Request env
            let ctx = cb.GetContext(s, request, env)
            s.Router.Route(request)
            |> Option.map (fun action ->
                let content = s.Controller.Handle(action)
                let response = Content.ToResponse content ctx |> Async.StartAsTask
                W2O.WriteResponse response env onException)
        with e ->
            Some (onException env e)

    type Assembly =

        static member LoadFileInfo(p: string) =
            let fn = Path.GetFullPath(p)
            let name = AssemblyName.GetAssemblyName(fn)
            match Assembly.TryLoad(name) with
            | None -> Assembly.LoadFrom(fn)
            | Some a -> a

        static member TryLoad(name: AssemblyName) =
            try
                match Assembly.Load(name) with
                | null -> None
                | a -> Some a
            with _ -> None

    let DiscoverAssemblies (path: string) =
        let ls pat = Directory.GetFiles(path, pat)
        let ( @ ) = Array.append
        ls "*.dll" @ ls "*.exe"
        |> Array.choose (fun p ->
            try Some (Assembly.LoadFileInfo(p))
            with e -> None)

type Options with

    static member Create(meta, graph) =
        let dir = System.IO.Directory.GetCurrentDirectory()
        let json = Core.Json.Provider.CreateTyped meta
        let remotingServer = Rem.Server.Create meta json
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
            if b then Some (Rem.Server.Create o.Metadata o.JsonProvider) else None
        { o with RemotingServer = server }

    static member Create(webRoot, ?binDirectory: string) =
        Options.Create(Shared.Metadata, Shared.Dependencies)
            .WithServerRootDirectory(webRoot)

type RemotingMiddleware(next: AppFunc, webRoot: string, server: Rem.Server, onException: Env -> exn -> Task, alwaysSetContext: bool) =

    member this.Invoke(env: Env) =
        let req = O2W.Request env
        if alwaysSetContext then O2W.SimpleContext webRoot env |> ignore
        let respHeaders = env.["owin.ResponseHeaders"] :?> HeaderDictionary
        let addRespHeader k v =
            respHeaders.[k] <-
                match respHeaders.TryGetValue k with
                | true, coll -> Array.append coll v
                | false, _ -> v
        let addRespHeaders headers =
            for k, vs in Seq.groupBy fst headers do
                addRespHeader k [| for _, v in vs -> v |]
        let getReqHeader k =
            match (env.["owin.RequestHeaders"] :?> HeaderDictionary).TryGetValue k with
            | true, xs when Array.length xs > 0 -> Some xs.[0]
            | _ -> None
        if Rem.IsRemotingRequest getReqHeader then
            async {
                try
                    match WebSharper.Web.RpcHandler.CorsAndCsrfCheck (unbox env.["owin.RequestMethod"]) req.Uri
                            (fun k -> match req.Cookies.[k] with null -> None | x -> Some x.Value)
                            getReqHeader
                            (fun k v ->
                                let expires = System.DateTime.UtcNow.AddYears(1000).ToString("R")
                                addRespHeader "Set-Cookie" [| sprintf "%s=%s; Expires=%s" k v expires |])
                        with
                    | WebSharper.Web.Error (code, msg) ->
                        env.["owin.ResponseStatusCode"] <- code
                        env.["owin.ResponseReasonPhrase"] <- msg
                        use w = new StreamWriter(env.["owin.ResponseBody"] :?> Stream)
                        w.Write msg
                    | WebSharper.Web.Preflight headers ->
                        addRespHeaders headers
                    | WebSharper.Web.Ok headers ->
                        addRespHeaders headers
                        let ctx = O2W.SimpleContext webRoot env
                        use reader = new StreamReader(req.Body)
                        let! body = reader.ReadToEndAsync() |> Async.AwaitTask
                        let! resp =
                            server.HandleRequest(
                                {
                                    Body = body
                                    Headers = getReqHeader
                                }, ctx)
                        env.["owin.ResponseStatusCode"] <- box 200
                        respHeaders.["Content-Type"] <- [|resp.ContentType|]
                        let bytes = Text.Encoding.UTF8.GetBytes(resp.Content)
                        (env.["owin.ResponseBody"] :?> Stream).Write(bytes, 0, bytes.Length)
                with e ->
                    return! onException env e |> Async.AwaitIAsyncResult |> Async.Ignore
            }
            |> Async.StartAsTask
            :> Task
        else next.Invoke(env)

    new (next, webRoot, server, onException) =
        new RemotingMiddleware(next, webRoot, server, onException, true)

    // (options)

    new (next, options: Options) =
        new RemotingMiddleware(next, options.ServerRootDirectory, options.RemotingServer.Value,
            options.OnException options.Debug)

    new (next, options: Options, alwaysSetContext) =
        new RemotingMiddleware(next, options.ServerRootDirectory, options.RemotingServer.Value,
            options.OnException options.Debug, alwaysSetContext)

    static member AsMidFunc(options: Options) =
        match options.RemotingServer with
        | Some rem ->
            MidFunc(fun next ->
                AppFunc(RemotingMiddleware(next, options.ServerRootDirectory, rem,
                            options.OnException options.Debug).Invoke))
        | None -> MidFunc(fun next -> AppFunc(fun env -> next.Invoke(env)))

    // (webRoot)

    static member UseRemoting(webRoot: string, ?binDirectory: string) =
        // Make sure that all relevant assemblies are loaded _before_ calling Shared.Metadata
        let _assemblies =
            match binDirectory with
            | Some d -> d
            | None -> Options.DefaultBinDirectory
            |> DiscoverAssemblies
        let o = Options.Create(Shared.Metadata, Shared.Dependencies).WithServerRootDirectory(webRoot)
        fun next -> new RemotingMiddleware(next, o)

    static member AsMidFunc(webRoot: string, ?binDirectory: string) =
        let mw = RemotingMiddleware.UseRemoting(webRoot)
        MidFunc(fun next -> AppFunc(mw(next).Invoke))

type SiteletMiddleware<'T when 'T : equality>(next: AppFunc, config: Options, sitelet: Sitelet<'T>) =
    let cb = ContextBuilder(config)

    let appFunc =
        let siteletAppFunc = AppFunc(fun env ->
            match dispatch cb sitelet env (config.OnException config.Debug) with
            | Some t -> t
            | None -> next.Invoke(env))
        if config.RemotingServer.IsSome then
            AppFunc(RemotingMiddleware(siteletAppFunc, config, false).Invoke)
        else
            siteletAppFunc

    member this.Invoke(env: Env) =
        appFunc.Invoke(env)

    static member AsMidFunc(config: Options, sitelet: Sitelet<'T>) =
        MidFunc(fun next ->
            let m = SiteletMiddleware(next, config, sitelet)
            AppFunc(m.Invoke))

    static member DiscoverSitelet(assemblies) =
        match HttpModule.DiscoverSitelet assemblies with
        | Some this -> this
        | None -> failwith "Failed to discover sitelet assemblies"

    static member UseDiscoveredSitelet(webRoot: string, ?binDirectory: string) =
        let binDirectory =
            match binDirectory with
            | None -> Options.DefaultBinDirectory
            | Some d -> d
        let assemblies = DiscoverAssemblies binDirectory
        let sitelet = SiteletMiddleware<'T>.DiscoverSitelet(assemblies)
        let options = Options.Create(webRoot)
        fun next -> SiteletMiddleware<obj>(next, options, sitelet)

    static member AsMidFunc(webRoot: string, ?binDirectory) =
        let mw = SiteletMiddleware<obj>.UseDiscoveredSitelet(webRoot, ?binDirectory = binDirectory)
        MidFunc(fun next -> AppFunc(mw(next).Invoke))

type InitAction = MidFunc * WebSharper.Core.Json.Provider * (Env -> Web.IContext) -> unit

type WebSharperOptions<'T when 'T : equality>() = 
    let mutable binDir = None
    member val ServerRootDirectory = System.IO.Directory.GetCurrentDirectory() with get, set
    member this.BinDirectory
        with get () =
            match binDir with
            | None -> Options.DefaultBinDirectory
            | Some dir -> dir
        and set dir = binDir <- Some dir
    member val UseRemoting = true with get, set
    member val UrlPrefix = "" with get, set
    member val Debug = false with get, set
    member val Sitelet = None with get, set
    member val DiscoverSitelet = false with get, set
    member this.DiscoverSiteletIn =
        if this.DiscoverSitelet then Some this.BinDirectory else None
    member val MetadataAndGraph = None with get, set
    member val OnException = Options.DefaultOnException with get, set

    static member DefaultOnException debug response exn =
        Options.DefaultOnException debug response exn

    member this.WithSitelet(sitelet: Sitelet<'T>) =
        this.Sitelet <- Some sitelet 
        this

    member this.BuildConfig() =
        // It's important to call DiscoverAssemblies before Shared.Metadata
        // because DiscoverAssemblies loads assemblies into the AppDomain
        // from which Metadata will then have to load metadata.
        let assemblies = DiscoverAssemblies this.BinDirectory

        let sitelet =
            if this.Sitelet.IsSome then
                Some (WebSharper.Sitelets.Sitelet.Upcast this.Sitelet.Value)
            elif this.DiscoverSitelet then
                HttpModule.DiscoverSitelet(assemblies)
            else None

        let meta, graph, json = 
            match this.MetadataAndGraph with
            | Some (m, g) -> m, g, Core.Json.Provider.CreateTyped m
            | None -> Shared.Metadata, Shared.Dependencies, Shared.Json
             
        let remotingServer =
            if this.UseRemoting then
                Rem.Server.Create meta json
                |> Some
            else None

        sitelet, {
            Debug = this.Debug
            JsonProvider = json
            Metadata = meta
            Dependencies = graph
            ServerRootDirectory = this.ServerRootDirectory
            UrlPrefix = this.UrlPrefix
            RemotingServer = remotingServer
            OnException = this.OnException
        }

    member this.AsMidFunc() =
        let sitelet, config = this.BuildConfig()
        match sitelet with
        | Some sitelet -> SiteletMiddleware<obj>.AsMidFunc(config, sitelet)
        | None -> RemotingMiddleware.AsMidFunc(config)

[<AutoOpen>]
module Extensions =
    type IAppBuilder with

        member this.UseWebSharperRemoting(webRoot: string, meta: M.Info) =
            this.Use(RemotingMiddleware.AsMidFunc(Options.Create(meta, DepG.FromData([ meta.Dependencies ])).WithServerRootDirectory(webRoot)))

        member this.UseWebSharperRemoting(webRoot: string, ?binDirectory: string) =
            this.Use(RemotingMiddleware.AsMidFunc(webRoot, ?binDirectory = binDirectory))

        member this.UseSitelet(webRoot: string, sitelet, ?binDirectory: string) =
            this.UseCustomSitelet(Options.Create(webRoot, ?binDirectory = binDirectory), sitelet)

        member this.UseCustomSitelet(config: Options, sitelet: Sitelet<'T>) =
            this.Use(SiteletMiddleware<'T>.AsMidFunc(config, sitelet))

        member this.UseDiscoveredSitelet(webRoot: string, ?binDirectory: string) =
            this.Use(SiteletMiddleware<obj>.AsMidFunc(webRoot, ?binDirectory = binDirectory))

        member this.UseWebSharper(options: WebSharperOptions<'T>) =
            this.Use(options.AsMidFunc())
