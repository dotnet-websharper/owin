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
open Microsoft.Owin
open WebSharper
open WebSharper.Sitelets
open WebSharper.Web
open System.Text

module Rem = WebSharper.Core.Remoting
module Res = WebSharper.Core.Resources
module P = WebSharper.PathConventions
module M = WebSharper.Core.Metadata

type Env = IDictionary<string, obj>
type AppFunc = Func<Env, Task>
type MidFunc = Func<AppFunc, AppFunc>

module EnvKey =

    let HttpContext = "HttpContext"
    module WebSharper =
        let Request = "WebSharper.Request"
        let Context = "WebSharper.Context"
        // Store WebSharper user identity in the environment dictionary,
        // avoid overwriting principal set by OWIN authentication middleware
        let User = "WebSharper.User"

    let GetOrSet<'T> (env: Env) (key: string) (mk: Env -> 'T) =
        match env.TryGetValue key with
        | true, (:? 'T as x) -> x
        | _ ->
            let x = mk env
            env.[key] <- x
            x

type Options =
    internal {
        Debug : bool
        JsonProvider : Core.Json.Provider
        Metadata : M.Info
        ServerRootDirectory : string
        UrlPrefix : string
        RemotingServer : option<Rem.Server>
        OnException : bool -> IOwinResponse -> exn -> Task
    }

    member o.WithDebug() = o.WithDebug(true)
    member o.WithDebug(d) = { o with Debug = d }
    member o.WithServerRootDirectory(d) = { o with ServerRootDirectory = d }
    member o.WithUrlPrefix(t) = { o with UrlPrefix = t }
    member o.WithOnException(f) = { o with OnException = f }

    static member DefaultOnException (debug: bool) (resp: IOwinResponse) (e: exn) =
        resp.StatusCode <- 500
        if debug then
            resp.WriteAsync (e.ToString())
        else
            resp.WriteAsync "Internal Server Error"

    static member Create() =
        let dir = System.IO.Directory.GetCurrentDirectory()
        {
            Debug = false
            JsonProvider = Core.Json.Provider.Create()
            Metadata = M.Info.Create([])
            ServerRootDirectory = dir
            UrlPrefix = ""
            RemotingServer = None
            OnException = Options.DefaultOnException
        }

    static member DefaultBinDirectory =
        Path.GetDirectoryName(typeof<Options>.Assembly.Location)

[<AutoOpen>]
module private Internal =

    open System.Reflection
    open HttpMultipartParser

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

    type OwinCookieUserSession(ctx: IOwinContext) =

        let refresh (cookie: string) =
            match cookie with
            | null -> ctx.Set(EnvKey.WebSharper.User, None)
            | cookie ->
                let ticket = FormsAuthentication.Decrypt cookie
                let principal = GenericPrincipal(FormsIdentity(ticket), [||])
                ctx.Set(EnvKey.WebSharper.User, Some principal)
            |> ignore 

        let ensureUserHasBeenRefreshed () = 
            if ctx.Environment.ContainsKey(EnvKey.WebSharper.User) |> not then 
                // Using `try ... with` because `FormsAuthentication.Decrypt`
                // throws an exception when there is a cookie but its format is invalid
                try refresh ctx.Request.Cookies.[FormsAuthentication.FormsCookieName]
                with _ -> refresh null

        interface IUserSession with

            member this.IsAvailable = true

            member this.GetLoggedInUser() =
                async {
                    ensureUserHasBeenRefreshed()
                    match ctx.Get<GenericPrincipal option>(EnvKey.WebSharper.User) with
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
                    let expires =
                        if persistent then
                            System.Nullable(cookie.Expires)
                        else System.Nullable()
                    ctx.Response.Cookies.Append(cookie.Name, cookie.Value,
                        CookieOptions(
                            Domain = cookie.Domain,
                            Expires = expires,
                            HttpOnly = cookie.HttpOnly,
                            Path = cookie.Path,
                            Secure = cookie.Secure))
                    return refresh cookie.Value
                }

            member this.Logout() =
                async {
                    ctx.Response.Cookies.Append(FormsAuthentication.FormsCookieName, "",
                        CookieOptions(Expires = System.Nullable(DateTime.Now.AddDays(-1.))))
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

        let Headers (headers: IHeaderDictionary) : seq<Http.Header> =
            seq {
                for KeyValue(k, vs) in headers do
                    for v in vs do
                        yield Http.Header.Custom k v
            }

        let Query (query: IReadableStringCollection) : Http.ParameterCollection =
            Http.ParameterCollection(
                seq {
                    for KeyValue(k, vs) in query do
                        for v in vs do
                            yield (k, v)
                }
            )

        let Cookies (cookies: RequestCookieCollection) : HttpCookieCollection =
            let coll = HttpCookieCollection()
            for KeyValue(k, v) in cookies do
                coll.Add(HttpCookie(k, v))
            coll

        let IsMultipart (req: IOwinRequest) =
            req.ContentType <> null &&
            req.ContentType.ToLowerInvariant().StartsWith "multipart/form-data"

        let DefaultCharset = System.Text.Encoding.GetEncoding("ISO-8859-1")
        let re = System.Text.RegularExpressions.Regex("; *charset *= *([^;]+)")
        let GetCharset (req: IOwinRequest) =
            match req.ContentType with
            | null -> DefaultCharset
            | s ->
                let m = re.Match(s.Trim())
                if m.Success then
                    System.Text.Encoding.GetEncoding m.Groups.[1].Value
                else DefaultCharset

        let ParseFormData (req: IOwinRequest) =
            let enc = GetCharset req
            let body = new MemoryStream()
            req.Body.CopyTo body
            body.Seek(0L, SeekOrigin.Begin) |> ignore
            if IsMultipart req then
                let parser = new MultipartFormDataParser(body, enc, leaveOpen = true)
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
            else
                use s = new StreamReader(body, enc, false, 1024, true)
                let q = System.Web.HttpUtility.ParseQueryString(s.ReadToEnd())
                { Files = []; Fields = Http.ParameterCollection(q); Body = body }

        let Request (req: IOwinRequest) : Http.Request =
            EnvKey.GetOrSet<Http.Request> req.Environment EnvKey.WebSharper.Request <| fun _ ->
            let formData = ParseFormData req
            formData.Body.Seek(0L, SeekOrigin.Begin) |> ignore
            let uri =
                match req.PathBase.Value with
                | "" | "/" -> req.Uri
                | pathBase ->
                    if req.Uri.IsAbsoluteUri then
                        let uB = UriBuilder req.Uri
                        if uB.Path.StartsWith pathBase then
                            uB.Path <- uB.Path.Substring pathBase.Length
                        uB.Uri
                    else
                        req.Uri
            {
                Method = Method req.Method
                Uri = uri
                Headers = Headers req.Headers
                Post = formData.Fields
                Get = Query req.Query
                Cookies = Cookies req.Cookies
                ServerVariables = Http.ParameterCollection([])
                Body = formData.Body
                Files = formData.Files
            }

        let SetHttpContext (env: Env) : unit =
            match HttpContext.Current with
            | null -> ()
            | x -> env.[EnvKey.HttpContext] <- HttpContextWrapper(x)

        let SimpleContext rootDir (req: IOwinRequest) : Web.IContext =
            let env = req.Environment
            EnvKey.GetOrSet<Web.IContext> env EnvKey.WebSharper.Context <| fun env ->
                SetHttpContext env
                let owinCtx = req.Context
                let uri = req.Uri
//                let wsReq = Request req
                let session = lazy new OwinCookieUserSession(owinCtx)
                { new IContext with
                    member ctx.Environment = env
                    member ctx.RequestUri = uri
                    member ctx.RootFolder = rootDir
                    member ctx.UserSession = session.Value :> _
                }

    module W2O =

        let WriteResponse (resp: Task<Http.Response>) (out: IOwinResponse) (onException: IOwinResponse -> exn -> Task) =
            resp.ContinueWith(fun (t: Task<Http.Response>) ->
                try
                    match t.Exception with
                    | null ->
                        let resp = t.Result
                        out.StatusCode <- resp.Status.Code
                        for name, hs in resp.Headers |> Seq.groupBy (fun h -> h.Name) do
                            out.Headers.AppendValues(name, [| for h in hs -> h.Value |])
                        let str = new MemoryStream()
                        resp.WriteBody(str :> _)
                        out.Write(str.ToArray())
                    | e ->
                        (onException out e).Wait()
                with e ->
                    (onException out e).Wait()
            )

    let buildResourceContext cfg (context: IOwinContext) : Res.Context =
        let isDebug = cfg.Debug
        let pu = P.PathUtility.VirtualPaths(context.Request.PathBase.Value)
        {
            DebuggingEnabled = isDebug
            DefaultToHttp = false
            GetSetting = fun (name: string) ->
                match ConfigurationManager.AppSettings.[name] with
                | null -> None
                | x -> Some x
            GetAssemblyRendering = fun name ->
                let aid = P.AssemblyId.Create(name.FullName)
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
        }

    [<Sealed>]
    type ContextBuilder(cfg) =
        let info = cfg.Metadata
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

        member b.GetContext<'T when 'T : equality>(site: Sitelet<'T>, req: Http.Request, context: IOwinContext) : Context<'T> =
            let appPath = context.Request.PathBase.Value
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
            EnvKey.GetOrSet<Context<'T>> context.Environment EnvKey.WebSharper.Context <| fun env ->
                O2W.SetHttpContext env
                {
                    ApplicationPath = appPath
                    Environment = env
                    Link = link
                    Json = json
                    Metadata = info
                    ResolveUrl = resolveUrl appPath
                    ResourceContext = resContext context
                    Request = req
                    RootFolder = cfg.ServerRootDirectory
                    UserSession = OwinCookieUserSession(context)
                }

    let dispatch (cb: ContextBuilder) (s: Sitelet<'T>) (context: IOwinContext) onException : option<Task> =
        try
            let request = O2W.Request context.Request
            let ctx = cb.GetContext(s, request, context)
            s.Router.Route(request)
            |> Option.map (fun action ->
                let content = s.Controller.Handle(action)
                let response = Content.ToResponse content ctx |> Async.StartAsTask
                W2O.WriteResponse response context.Response onException)
        with e ->
            Some (onException context.Response e)

    type Assembly =

        static member LoadFileInfo(p: string) =
            let fn = Path.GetFullPath p
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
        let files = Array.append (ls "*.dll") (ls "*.exe")
        files |> Array.choose (fun p ->
            try Some (Assembly.LoadFileInfo(p))
            with e -> None)

    type M.Info with

        static member LoadFromAssemblies(assemblies: Reflection.Assembly[]) =
            assemblies
            |> Seq.choose (fun f -> try M.AssemblyInfo.LoadReflected f with _ -> None)
            |> M.Info.Create

        static member LoadFromBinDirectory(binDirectory: string) =
            DiscoverAssemblies binDirectory
            |> Seq.choose (fun f -> try M.AssemblyInfo.Load(f.FullName) with _ -> None)
            |> M.Info.Create

        static member LoadFromWebRoot(webRoot: string) =
            M.Info.LoadFromBinDirectory(Path.Combine(webRoot, "bin"))

type Options with

    static member Create(meta) =
        let dir = System.IO.Directory.GetCurrentDirectory()
        let remotingServer = Rem.Server.Create None meta
        {
            Debug = false
            JsonProvider = remotingServer.JsonProvider
            Metadata = meta
            ServerRootDirectory = dir
            UrlPrefix = ""
            RemotingServer = Some remotingServer
            OnException = Options.DefaultOnException
        }

    member o.WithRunRemoting(b) =
        let server =
            if b then Some (Rem.Server.Create None o.Metadata) else None
        { o with RemotingServer = server }

    static member Create(webRoot, ?binDirectory) =
        let binDirectory = defaultArg binDirectory (Path.Combine(webRoot, "bin"))
        let meta = M.Info.LoadFromBinDirectory(binDirectory)
        Options.Create(meta)
            .WithServerRootDirectory(webRoot)

    member o.WithBinDirectory(dir) =
        let meta = M.Info.LoadFromBinDirectory dir
        { o with Metadata = meta }

type RemotingMiddleware(next: AppFunc, webRoot: string, server: Rem.Server, onException: IOwinResponse -> exn -> Task, alwaysSetContext: bool) =

    member this.Invoke(env: Env) =
        let context = OwinContext(env) :> IOwinContext
        let httpContext = HttpContext.Current
        if alwaysSetContext then O2W.SimpleContext webRoot context.Request |> ignore
        let headers =
            O2W.Headers context.Request.Headers
            |> Seq.map (fun h -> (h.Name.ToLowerInvariant(), h.Value))
            |> Map.ofSeq
        let getReqHeader (k: string) =
            Map.tryFind (k.ToLowerInvariant()) headers
        let addRespHeaders headers =
            headers |> List.iter (fun (k, v) -> context.Response.Headers.Add(k, [|v|]))
        if Rem.IsRemotingRequest getReqHeader then
            async {
                try
                    match WebSharper.Web.RpcHandler.CorsAndCsrfCheck context.Request.Method context.Request.Uri
                            (fun k -> match context.Request.Cookies.[k] with null -> None | x -> Some x)
                            getReqHeader
                            (fun k v -> context.Response.Cookies.Append(k, v,
                                            CookieOptions(Expires = Nullable(System.DateTime.UtcNow.AddYears(1000)))))
                            with
                    | WebSharper.Web.Error (code, _, body) ->
                        context.Response.StatusCode <- code
                        context.Response.Write(body)
                    | WebSharper.Web.Preflight headers ->
                        addRespHeaders headers
                    | WebSharper.Web.Ok headers ->
                        addRespHeaders headers
                        let ctx = O2W.SimpleContext webRoot context.Request
                        use reader = new StreamReader(context.Request.Body)
                        let! body = reader.ReadToEndAsync() |> Async.AwaitTask
                        let! resp =
                            server.HandleRequest(
                                {
                                    Body = body
                                    Headers = getReqHeader
                                }, ctx)
                        context.Response.StatusCode <- 200
                        context.Response.ContentType <- resp.ContentType
                        let bytes = Encoding.UTF8.GetBytes(resp.Content)
                        context.Response.Write(bytes, 0, bytes.Length)
                with e ->
                    return! onException context.Response e |> Async.AwaitIAsyncResult |> Async.Ignore
            }
            |> Async.StartAsTask
            :> Task
        else next.Invoke(env)

    new (next, webRoot, server, onException) =
        new RemotingMiddleware(next, webRoot, server, onException, true)

    // (options)

    new (next, options: Options) =
        new RemotingMiddleware(next, options, true)

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

    // (webRoot, ?binDirectory)

    static member UseRemoting(webRoot: string, ?binDirectory: string) =
        let meta =
            match binDirectory with
            | None -> M.Info.LoadFromWebRoot(webRoot)
            | Some binDirectory -> M.Info.LoadFromBinDirectory(binDirectory)
        let o = Options.Create(meta).WithServerRootDirectory(webRoot)
        fun next -> new RemotingMiddleware(next, o)

    static member AsMidFunc(webRoot: string, ?binDirectory: string) =
        let mw = RemotingMiddleware.UseRemoting(webRoot, ?binDirectory = binDirectory)
        MidFunc(fun next -> AppFunc(mw(next).Invoke))

type SiteletMiddleware<'T when 'T : equality>(next: AppFunc, config: Options, sitelet: Sitelet<'T>) =
    let cb = ContextBuilder(config)

    let appFunc =
        let siteletAppFunc = AppFunc(fun env ->
            let context = OwinContext(env) :> IOwinContext
            match dispatch cb sitelet context (config.OnException config.Debug) with
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
        let binDir =
            match binDirectory with
            | None -> Options.DefaultBinDirectory
            | Some d -> d
        let options = Options.Create(binDir)
        let ok =
            try
                DiscoverAssemblies binDir
                |> HttpModule.DiscoverSitelet
                |> Option.map (fun sitelet ->
                    fun next -> SiteletMiddleware<obj>(next, options, sitelet))
            with :? System.Reflection.ReflectionTypeLoadException as exn ->
                failwithf "%A" exn.LoaderExceptions
        match ok with
        | Some this -> this
        | None -> failwith "Failed to discover sitelet assemblies"

    static member AsMidFunc(webRoot: string, ?binDirectory: string) =
        let mw = SiteletMiddleware<obj>.UseDiscoveredSitelet(webRoot, ?binDirectory = binDirectory)
        MidFunc(fun next -> AppFunc(mw(next).Invoke))

type WebSharperOptions<'T when 'T : equality>() = 
    let mutable binDir = None
    member val ServerRootDirectory = System.IO.Directory.GetCurrentDirectory() with get, set
    member this.BinDirectory
        with get () = 
            match binDir with
            | None -> Options.DefaultBinDirectory
            | Some d -> d
        and set dir = binDir <- Some dir
    member val UseRemoting = true with get, set
    member val UrlPrefix = "" with get, set
    member val Debug = false with get, set
    member val Sitelet = None with get, set
    member val DiscoverSitelet = false with get, set
    member this.DiscoverSiteletIn =
        if this.DiscoverSitelet then Some this.BinDirectory else None
    member val Metadata = None with get, set
    member val OnException = Options.DefaultOnException with get, set

    static member DefaultOnException debug response exn =
        Options.DefaultOnException debug response exn

    member this.WithSitelet(sitelet: Sitelet<'T>) =
        this.Sitelet <- Some sitelet 
        this
        
    member this.BuildConfig() =
        let assemblies = DiscoverAssemblies this.BinDirectory

        let sitelet =
            match this.Sitelet with
            | Some s -> Some (WebSharper.Sitelets.Sitelet.Upcast s)
            | None when this.DiscoverSitelet -> HttpModule.DiscoverSitelet(assemblies)
            | None -> None

        let meta =
            match this.Metadata with
            | Some m -> m
            | None ->
                if this.UseRemoting || Option.isSome this.Sitelet || this.DiscoverSitelet then
                    M.Info.LoadFromAssemblies(assemblies)
                else
                    M.Info.Create([])

        let remotingServer, jsonProvider =
            if this.UseRemoting then
                let rem = Rem.Server.Create None meta
                Some rem, rem.JsonProvider
            else None, Core.Json.Provider.Create()

        sitelet, {
            Debug = this.Debug
            JsonProvider = jsonProvider
            Metadata = meta
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
            this.Use(RemotingMiddleware.AsMidFunc(Options.Create(meta).WithServerRootDirectory(webRoot)))

        member this.UseWebSharperRemoting(meta: M.Info) =
            this.Use(RemotingMiddleware.AsMidFunc(Options.Create(meta)))

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
            this.Use(options.AsMidFunc())
