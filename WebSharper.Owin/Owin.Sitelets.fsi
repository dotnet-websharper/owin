namespace WebSharper.Owin

open System
open System.Collections.Generic
open System.Threading.Tasks
open WebSharper
open WebSharper.Sitelets
module M = WebSharper.Core.Metadata

/// Options to initialize a sitelet server with IAppBuilder.UseCustomSitelet.
[<Sealed>]
type Options =
    /// Creates empty sitelet server options.
    static member Create : unit -> Options

    /// Creates sitelet server options with the given WebSharper metadata.
    static member Create : M.Info -> Options

    /// Creates sitelet server options using webRoot as the root folder and
    /// loading WebSharper metadata from assemblies in binDirectory.
    /// If binDirectory is not specified, webRoot/bin is used.
    static member Create : webRoot: string * ?binDirectory: string -> Options

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

type Env = IDictionary<string, obj>
type AppFunc = Func<Env, Task>
type MidFunc = Func<AppFunc, AppFunc>

type RemotingMiddleware =

    member Invoke : Env -> Task

    /// Runs the WebSharper Remoting service, allowing WebSharper-compiled
    /// client-side code to invoke [<Rpc>]-annotated server-side functions.
    new : next: AppFunc * Options -> RemotingMiddleware
    /// Runs the WebSharper Remoting service, allowing WebSharper-compiled
    /// client-side code to invoke [<Rpc>]-annotated server-side functions.
    static member AsMidFunc : Options -> MidFunc

    /// Runs the WebSharper Remoting service, allowing WebSharper-compiled
    /// client-side code to invoke [<Rpc>]-annotated server-side functions.
    /// WebSharper metadata is loaded from binDirectory.
    /// If binDirectory is not specified, webRoot/bin is used.
    new : next: AppFunc * webRoot: string * ?binDirectory: string -> RemotingMiddleware
    /// Runs the WebSharper Remoting service, allowing WebSharper-compiled
    /// client-side code to invoke [<Rpc>]-annotated server-side functions.
    /// WebSharper metadata is loaded from binDirectory.
    /// If binDirectory is not specified, webRoot/bin is used.
    static member AsMidFunc : webRoot: string * ?binDirectory: string -> MidFunc

type SiteletMiddleware<'T when 'T : equality> =

    member Invoke : Env -> Task

    /// Runs the provided Sitelet with the provided options.
    new : next: AppFunc * config: Options * sitelet: Sitelet<'T> -> SiteletMiddleware<'T>
    /// Runs the provided Sitelet with the provided options.
    static member AsMidFunc : config: Options * sitelet: Sitelet<'T> -> MidFunc

    /// Inspects the binDirectory folder, looking for an assembly that contains
    /// a WebSharper Sitelet, and runs this Sitelet with webRoot as the root folder.
    /// Also runs the Remoting service using metadata discovered from binDirectory.
    /// If binDirectory is not specified, webRoot/bin is used.
    static member UseDiscoveredSitelet : webRoot: string * ?binDirectory: string -> (AppFunc -> SiteletMiddleware<obj>)
    /// Inspects the binDirectory folder, looking for an assembly that contains
    /// a WebSharper Sitelet, and runs this Sitelet with webRoot as the root folder.
    /// Also runs the Remoting service using metadata discovered from binDirectory.
    /// If binDirectory is not specified, webRoot/bin is used.
    static member AsMidFunc : webRoot: string * ?binDirectory: string -> MidFunc

[<AutoOpen>]
module Extensions =

    open global.Owin

    type IAppBuilder with
        /// Inspects the binDirectory folder, looking for an assembly that contains
        /// a WebSharper Sitelet, and runs this Sitelet with webRoot as the root folder.
        /// Also runs the Remoting service using metadata discovered from binDirectory.
        /// If binDirectory is not specified, webRoot/bin is used.
        member UseDiscoveredSitelet : webRoot: string * ?binDirectory: string -> IAppBuilder

        /// Runs the provided Sitelet with webRoot as the root folder, using
        /// WebSharper metadata loaded from assemblies located in binDirectory.
        /// Also runs the Remoting service using metadata discovered from binDirectory.
        /// If binDirectory is not specified, webRoot/bin is used.
        member UseSitelet : webRoot: string * Sitelet<'T> * ?binDirectory: string -> IAppBuilder

        /// Runs the provided Sitelet with the provided options.
        member UseCustomSitelet : Options * Sitelet<'T> -> IAppBuilder

        /// Runs the WebSharper Remoting service, allowing WebSharper-compiled
        /// client-side code to invoke [<Rpc>]-annotated server-side functions.
        /// Note that the Remoting service is automatically run by the
        /// methods UseDiscoveredSitelet and UseSitelet, as well as
        /// UseCustomSitelet if options.RunRemoting is set to true.
        /// WebSharper metadata is loaded from binDirectory.
        /// If binDirectory is not specified, webRoot/bin is used.
        member UseWebSharperRemoting : webRoot: string * ?binDirectory: string -> IAppBuilder

        /// Runs the WebSharper Remoting service, allowing WebSharper-compiled
        /// client-side code to invoke [<Rpc>]-annotated server-side functions.
        /// Note that the Remoting service is automatically run by the
        /// methods UseDiscoveredSitelet and UseSitelet, as well as
        /// UseCustomSitelet if options.RunRemoting is set to true.
        /// WebSharper metadata is loaded from binDirectory.
        [<System.Obsolete "Use UseWebSharperRemoting(webRoot, binDirectory).">]
        member UseWebSharperRemotingFromBin : binDirectory: string -> IAppBuilder

        /// Runs the WebSharper Remoting service, allowing WebSharper-compiled
        /// client-side code to invoke [<Rpc>]-annotated server-side functions.
        /// The current working directory is used as application root.
        /// Note that the Remoting service is automatically run by the
        /// methods UseDiscoveredSitelet and UseSitelet, as well as
        /// UseCustomSitelet if options.RunRemoting is set to true.
        [<System.Obsolete "Use UseWebSharperRemoting(webRoot, info).">]
        member UseWebSharperRemoting : M.Info -> IAppBuilder

        /// Runs the WebSharper Remoting service, allowing WebSharper-compiled
        /// client-side code to invoke [<Rpc>]-annotated server-side functions.
        /// Note that the Remoting service is automatically run by the
        /// methods UseDiscoveredSitelet and UseSitelet, as well as
        /// UseCustomSitelet if options.RunRemoting is set to true.
        member UseWebSharperRemoting : webRoot: string * M.Info -> IAppBuilder
