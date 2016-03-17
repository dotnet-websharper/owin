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
module Rem = WebSharper.Core.Remoting
module Res = WebSharper.Core.Resources
module P = WebSharper.PathConventions
module M = WebSharper.Core.Metadata
#if ZAFIR
type DepG = WebSharper.Core.DependencyGraph.Graph
#endif

type Env = IDictionary<string, obj>
type AppFunc = Func<Env, Task>
type MidFunc = Func<AppFunc, AppFunc>

type Options =
    internal {
        Debug : bool
        JsonProvider : Core.Json.Provider
        Metadata : M.Info
        ServerRootDirectory : string
        UrlPrefix : string
        RemotingServer : option<Rem.Server>
    }

    member o.WithDebug() = o.WithDebug(true)
    member o.WithDebug(d) = { o with Debug = d }
    member o.WithServerRootDirectory(d) = { o with ServerRootDirectory = d }
    member o.WithUrlPrefix(t) = { o with UrlPrefix = t }

    static member Create() =
        let dir = System.IO.Directory.GetCurrentDirectory()
        {
            Debug = false
            JsonProvider = Core.Json.Provider.Create()
#if ZAFIR
            Metadata = M.Info.Empty
#else
            Metadata = M.Info.Create([])
#endif
            ServerRootDirectory = dir
            UrlPrefix = ""
            RemotingServer = None
        }

[<AutoOpen>]
module private Internal =

    open System.Reflection
    open HttpMultipartParser

    let [<Literal>] OwinContextKey = "OwinContext"
    let [<Literal>] HttpContextKey = "HttpContext"

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
                let parser = new MultipartFormDataParser(body, enc)
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

    module W2O =

        let WriteResponse (resp: Task<Http.Response>) (out: IOwinResponse) =
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
                        out.Write(sprintf "%A" e)
                with e ->
                    out.Write(sprintf "%A" e)
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
        }

    // Store WebSharper user identity in the environment dictionary,
    // avoid overwriting principal set by OWIN authentication middleware
    let [<Literal>] WebSharperUserKey = "WebSharper.User"

    type OwinCookieUserSession(ctx: IOwinContext) =

        let refresh (cookie: string) =
            match cookie with
            | null -> ctx.Set(WebSharperUserKey, None)
            | cookie ->
                let ticket = FormsAuthentication.Decrypt cookie
                let principal = GenericPrincipal(FormsIdentity(ticket), [||])
                ctx.Set(WebSharperUserKey, Some principal)
            |> ignore 

        let ensureUserHasBeenRefreshed () = 
            if ctx.Environment.ContainsKey(WebSharperUserKey) |> not then 
                // Using `try ... with` because `FormsAuthentication.Decrypt`
                // throws an exception when there is a cookie but its format is invalid
                try refresh ctx.Request.Cookies.[FormsAuthentication.FormsCookieName]
                with _ -> refresh null

        interface IUserSession with

            member this.IsAvailable = true

            member this.GetLoggedInUser() =
                async {
                    ensureUserHasBeenRefreshed()
                    match ctx.Get<GenericPrincipal option>(WebSharperUserKey) with
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

    let mkEnv (owinContext: IOwinContext) (httpContext: HttpContext) =
        let m = Map.add OwinContextKey (box owinContext) Map.empty
        match httpContext with
        | null -> m
        | x -> Map.add HttpContextKey (box (HttpContextWrapper(x))) m

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

        let resolveUrl context appPath u =
            if VirtualPathUtility.IsAppRelative(u) then
                VirtualPathUtility.ToAbsolute(u, appPath)
            else
                u

        member b.GetContext<'T when 'T : equality>(site: Sitelet<'T>, req: Http.Request, context: IOwinContext, httpContext: HttpContext) : Context<'T> =
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
            {
                ApplicationPath = appPath
                Environment = mkEnv context httpContext
                Link = link
                Json = json
                Metadata = info
                ResolveUrl = resolveUrl context appPath
                ResourceContext = resContext context
                Request = req
                RootFolder = cfg.ServerRootDirectory
                UserSession = OwinCookieUserSession(context)
            }

    let dispatch (cb: ContextBuilder) (s: Sitelet<'T>) (context: IOwinContext) (httpContext: HttpContext) : option<Task> =
        try
            let request = O2W.Request context.Request
            let ctx = cb.GetContext(s, request, context, httpContext)
            s.Router.Route(request)
            |> Option.map (fun action ->
                let content = s.Controller.Handle(action)
                let response = Content.ToResponse content ctx |> Async.StartAsTask
                W2O.WriteResponse response context.Response)
        with e ->
            context.Response.StatusCode <- 500
            Some (context.Response.WriteAsync(sprintf "%A" e))

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
            d.DiscoverAssemblies()
#if ZAFIR
            |> Seq.choose (fun f -> try M.IO.LoadReflected(Assembly.LoadFileInfo f) with _ -> None)
            |> DepG.UnionOfMetadata
#else
            |> Seq.choose (fun f -> try M.AssemblyInfo.Load(f.FullName) with _ -> None)
            |> M.Info.Create
#endif

        static member LoadFromWebRoot(webRoot: string) =
            M.Info.LoadFromBinDirectory(Path.Combine(webRoot, "bin"))

type Options with

    static member Create(meta) =
        let dir = System.IO.Directory.GetCurrentDirectory()
#if ZAFIR
        let remotingServer = Rem.Server.Create meta
#else
        let remotingServer = Rem.Server.Create None meta
#endif
        {
            Debug = false
            JsonProvider = remotingServer.JsonProvider
            Metadata = meta
            ServerRootDirectory = dir
            UrlPrefix = ""
            RemotingServer = Some remotingServer
        }

    member o.WithRunRemoting(b) =
        let server =
#if ZAFIR
            if b then Some (Rem.Server.Create o.Metadata) else None
#else
            if b then Some (Rem.Server.Create None o.Metadata) else None
#endif
        { o with RemotingServer = server }

    static member Create(webRoot, ?binDirectory) =
        let binDirectory = defaultArg binDirectory (Path.Combine(webRoot, "bin"))
        let meta = M.Info.LoadFromBinDirectory(binDirectory)
        Options.Create(meta)
            .WithServerRootDirectory(webRoot)

    member o.WithBinDirectory(dir) =
        let meta = M.Info.LoadFromBinDirectory dir
        { o with Metadata = meta }

type RemotingMiddleware(next: AppFunc, webRoot: string, server: Rem.Server) =

    member this.Invoke(env: Env) =
        let context = OwinContext(env) :> IOwinContext
        let httpContext = HttpContext.Current
        let headers =
            O2W.Headers context.Request.Headers
            |> Seq.map (fun h -> (h.Name, h.Value))
            |> Map.ofSeq
        let getHeader k =
            Map.tryFind k headers
        if Rem.IsRemotingRequest getHeader then
            async {
                try
                    use reader = new StreamReader(context.Request.Body)
                    let session = new OwinCookieUserSession(context)
                    let uri = context.Request.Uri
                    let ctx =
                        { new Web.IContext with
                            member this.UserSession = session :> _
                            member this.RequestUri = uri
                            member this.RootFolder = webRoot
                            member this.Environment = upcast mkEnv context httpContext}
                    let! body = reader.ReadToEndAsync() |> Async.AwaitTask
                    let! resp =
                        server.HandleRequest(
                            {
                                Body = body
                                Headers = getHeader
                            }, ctx)
                    context.Response.StatusCode <- 200
                    context.Response.ContentType <- resp.ContentType
                    context.Response.Write(resp.Content)
                with e ->
                    context.Response.StatusCode <- 500
                    context.Response.Write(sprintf "%A" e)
                return ()
            }
            |> Async.StartAsTask
            :> Task
        else next.Invoke(env)

    // (options)

    new (next, options: Options) =
        new RemotingMiddleware(next, options.ServerRootDirectory, options.RemotingServer.Value)

    static member AsMidFunc(options: Options) =
        MidFunc(fun next ->
            AppFunc(RemotingMiddleware(next, options).Invoke))

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
            let httpContext = HttpContext.Current // non-null if running on top of asp.net
            match dispatch cb sitelet context httpContext with
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

type InitAction = Owin.IAppBuilder * WebSharper.Core.Json.Provider * (IOwinContext -> Web.IContext) -> unit

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
    member val Metadata = None with get, set
    
    member internal this.InitActions = initActions

    member this.WithSitelet(sitelet: Sitelet<'T>) =
        this.Sitelet <- Some sitelet 
        this

    member this.WithInitAction(action) = 
        initActions <- action :: initActions   
        this  

    member internal this.Run(builder: IAppBuilder) =
        let meta = 
            match this.Metadata with
            | Some m -> m
            | None ->
                if this.UseRemoting || Option.isSome this.Sitelet || this.DiscoverSitelet then
                    M.Info.LoadFromBinDirectory(this.BinDirectory)
                else
#if ZAFIR
                    M.Info.Empty
#else
                    M.Info.Create([])
#endif
             
        let remotingServer, jsonProvider =
            if this.UseRemoting then
#if ZAFIR
                let rem = Rem.Server.Create meta
#else
                let rem = Rem.Server.Create None meta
#endif
                Some rem, rem.JsonProvider
            else None, Core.Json.Provider.Create()

        let config = {
            Debug = this.Debug
            JsonProvider = jsonProvider
            Metadata = meta
            ServerRootDirectory = this.ServerRootDirectory
            UrlPrefix = this.UrlPrefix
            RemotingServer = remotingServer
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
            let mkCtx (context: IOwinContext) =
                let session = new OwinCookieUserSession(context)
                { new IContext with
                    member ctx.Environment = upcast mkEnv context HttpContext.Current
                    member ctx.RequestUri = context.Request.Uri
                    member ctx.RootFolder = this.ServerRootDirectory
                    member ctx.UserSession = session :> _
                }
            for a in this.InitActions do
                a(builder, jsonProvider, mkCtx)

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
            options.Run(this)
            this
