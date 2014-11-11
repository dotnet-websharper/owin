namespace IntelliFactory.WebSharper.Owin

open IntelliFactory.WebSharper
module M = IntelliFactory.WebSharper.Core.Metadata

/// Options to initialize a sitelet server with IAppBuilder.UseCustomSitelet.
[<Sealed>]
type Options =
    /// Creates empty sitelet server options.
    static member Create : unit -> Options

    /// Creates sitelet server options with the given WebSharper metadata.
    static member Create : M.Info -> Options

    /// Creates sitelet server options from WebSharper metadata loaded from
    /// assemblies in the "bin" subfolder of webRoot.
    static member Create : webRoot: string -> Options

    /// Enables debugging (including using uncompressed JavaScript files).
    member WithDebug : unit -> Options

    /// Enables or disables debugging (including using uncompressed
    /// JavaScript files).
    member WithDebug : bool -> Options

    /// Sets the root folder for the application.
    member WithServerRootDirectory : string -> Options

    /// Sets the URL prefix under which the sitelet is served.
    member WithUrlPrefix : string -> Options

    /// Sets whether the WebSharper Remoting service should be run.
    member WithRunRemoting : bool -> Options

[<AutoOpen>]
module Extensions =

    open global.Owin
    open IntelliFactory.WebSharper.Sitelets

    type IAppBuilder with
        /// Inspects the webRoot folder, looking for an assembly in the "bin"
        /// subfolder that contains a WebSharper Sitelet, and runs this Sitelet
        /// with webRoot as the root folder.
        member UseDiscoveredSitelet : webRoot: string -> IAppBuilder

        /// Runs the provided Sitelet with webRoot as the root folder, using
        /// WebSharper metadata loaded from assemblies located in the "bin"
        /// subfolder of webRoot.
        member UseSitelet : webRoot: string * Sitelet<'T> -> IAppBuilder

        /// Runs the provided Sitelet. Allows a more customized setup than the
        /// previous methods, for example running a Sitelet whose code isn't
        /// located in the "bin" subfolder of the root folder, or running the
        /// Sitelet with a URL prefix.
        member UseCustomSitelet : Options * Sitelet<'T> -> IAppBuilder

        /// Runs the WebSharper Remoting service, allowing WebSharper-compiled
        /// client-side code to invoke [<Rpc>]-annotated server-side functions.
        /// Note that the Remoting service is automatically run by the above
        /// methods UseDiscoveredSitelet and UseSitelet, as well as
        /// UseCustomSitelet if options.RunRemoting is set to true.
        member UseWebSharperRemoting : webRoot: string -> IAppBuilder

        /// Runs the WebSharper Remoting service, allowing WebSharper-compiled
        /// client-side code to invoke [<Rpc>]-annotated server-side functions.
        /// Note that the Remoting service is automatically run by the above
        /// methods UseDiscoveredSitelet and UseSitelet, as well as
        /// UseCustomSitelet if options.RunRemoting is set to true.
        member UseWebSharperRemoting : M.Info -> IAppBuilder
