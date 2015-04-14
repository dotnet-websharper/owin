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

type Env = IDictionary<string, obj>
type AppFunc = Func<Env, Task>
type MidFunc = Func<AppFunc, AppFunc>

type Options =
    private {
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
            Metadata = M.Info.Create([])
            ServerRootDirectory = dir
            UrlPrefix = ""
            RemotingServer = None
        }

[<AutoOpen>]
module private Internal =

    open System.Reflection
    open HttpMultipartParser

    type FormData =
        {
            Files : seq<HttpPostedFileBase>
            Fields : Http.ParameterCollection
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
            req.ContentType.ToLower().StartsWith "multipart/form-data"

        let ParseMultiPartFormData (req: IOwinRequest) =
            if IsMultipart req then
                let parser = new MultipartFormDataParser(req.Body)
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
                                    use ms = new MemoryStream()
                                    let buffer = Array.zeroCreate (16 * 1024)
                                    let rec loop () =
                                        let read = f.Data.Read(buffer, 0, buffer.Length)
                                        if read > 0 then ms.Write(buffer, 0, read); loop ()
                                    loop ()
                                    File.WriteAllBytes(filename, ms.ToArray()) }
                    |]
                { Files = files; Fields = Http.ParameterCollection(fields) }
            else
                { Files = []; Fields = Http.ParameterCollection([]) }

        let Request (req: IOwinRequest) : Http.Request =
            let formData = ParseMultiPartFormData req
            {
                Method = Method req.Method
                Uri = req.Uri
                Headers = Headers req.Headers
                Post = formData.Fields
                Get = Query req.Query
                Cookies = Cookies req.Cookies
                ServerVariables = Http.ParameterCollection([])
                Body = req.Body
                Files = formData.Files
            }

    module W2O =

        let WriteResponse (resp: Http.Response) (out: IOwinResponse) =
            out.StatusCode <- resp.Status.Code
            for name, hs in resp.Headers |> Seq.groupBy (fun h -> h.Name) do
                out.Headers.Add(name, [| for h in hs -> h.Value |])
            let str = new MemoryStream()
            resp.WriteBody(str :> _)
            out.WriteAsync(str.ToArray())

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

    type OwinCookieUserSession(ctx: IOwinContext) =

        let refresh (cookie: string) =
            match cookie with
            | null -> ctx.Authentication.User <- null
            | cookie ->
                let ticket = FormsAuthentication.Decrypt cookie
                let principal = GenericPrincipal(FormsIdentity(ticket), [||])
                ctx.Authentication.User <- principal

        do  // Using `try ... with` because `FormsAuthentication.Decrypt`
            // throws an exception when there is a cookie but its format is invalid
            try refresh ctx.Request.Cookies.[FormsAuthentication.FormsCookieName]
            with _ -> refresh null

        interface IUserSession with

            member this.IsAvailable = true

            member this.GetLoggedInUser() =
                async {
                    match ctx.Authentication.User with
                    | null -> return None
                    | x ->
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
            {
                ApplicationPath = appPath
                Link = link
                Json = json
                Metadata = info
                ResolveUrl = resolveUrl context appPath
                ResourceContext = resContext context
                Request = req
                RootFolder = cfg.ServerRootDirectory
                UserSession = OwinCookieUserSession(context)
            }

    let dispatch (cb: ContextBuilder) (s: Sitelet<'T>) (context: IOwinContext) : option<Task> =
        try
            let request = O2W.Request context.Request
            let ctx = cb.GetContext(s, request, context)
            s.Router.Route(request)
            |> Option.map (fun action ->
                let content = s.Controller.Handle(action)
                let response = Content.ToResponse content ctx
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
            |> Seq.choose (fun f -> M.AssemblyInfo.Load(f.FullName))
            |> M.Info.Create

        static member LoadFromWebRoot(webRoot: string) =
            M.Info.LoadFromBinDirectory(Path.Combine(webRoot, "bin"))

type Options with

    static member Create(meta) =
        let dir = System.IO.Directory.GetCurrentDirectory()
        {
            Debug = false
            JsonProvider = Core.Json.Provider.CreateTyped(meta)
            Metadata = meta
            ServerRootDirectory = dir
            UrlPrefix = ""
            RemotingServer = Some (Rem.Server.Create None meta)
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

type RemotingMiddleware(next: AppFunc, webRoot: string, server: Rem.Server) =

    member this.Invoke(env: Env) =
        let context = OwinContext(env) :> IOwinContext
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
                    let! body = reader.ReadToEndAsync() |> Async.AwaitTask
                    let! resp =
                        server.WithContext(fun _ ->
                            let session = new OwinCookieUserSession(context)
                            let uri = context.Request.Uri
                            {
                                new Web.IContext with
                                    member this.UserSession = session :> _
                                    member this.RequestUri = uri
                                    member this.RootFolder = webRoot
                            }
                        ).HandleRequest {
                            Body = body
                            Headers = getHeader
                        }
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

    new (next, webRoot: string, ?binDirectory: string) =
        let meta =
            match binDirectory with
            | None -> M.Info.LoadFromWebRoot(webRoot)
            | Some binDirectory -> M.Info.LoadFromBinDirectory(binDirectory)
        let o = Options.Create(meta).WithServerRootDirectory(webRoot)
        new RemotingMiddleware(next, o)

    static member AsMidFunc(webRoot: string, ?binDirectory: string) =
        MidFunc(fun next ->
            AppFunc(RemotingMiddleware(next, webRoot, ?binDirectory = binDirectory).Invoke))

type SiteletMiddleware<'T when 'T : equality>(next: AppFunc, config: Options, sitelet: Sitelet<'T>) =
    let cb = ContextBuilder(config)

    let appFunc =
        let siteletAppFunc = AppFunc(fun env ->
            let context = OwinContext(env) :> IOwinContext
            match dispatch cb sitelet context with
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
            binDir.DiscoverAssemblies()
            |> Seq.choose (fun p ->
                try Some (Assembly.LoadFileInfo(p))
                with e -> None)
            |> Array.ofSeq
            |> Seq.tryPick (fun assem ->
                let aT = typeof<WebsiteAttribute>
                match Attribute.GetCustomAttribute(assem, aT) with
                | :? WebsiteAttribute as attr ->
                    let (sitelet, actions) = attr.Run()
                    let options = Options.Create(webRoot, binDirectory = binDirectory)
                    fun next ->
                        SiteletMiddleware<obj>(next, options, sitelet)
                    |> Some
                | _ -> None)
        match ok with
        | Some this -> this
        | None -> failwith "Failed to discover sitelet assemblies"

    static member AsMidFunc(webRoot: string, ?binDirectory: string) =
        let mw = SiteletMiddleware<obj>.UseDiscoveredSitelet(webRoot, ?binDirectory = binDirectory)
        MidFunc(fun next -> AppFunc(mw(next).Invoke))

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
