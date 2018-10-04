// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2018 IntelliFactory
//
// Licensed under the Apache License, Version 2.0 (the "License"); you
// may not use this file except in compliance with the License.  You may
// obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied.  See the License for the specific language governing
// permissions and limitations under the License.
//
// $end{copyright}
namespace WebSharper.Owin

open System
open System.Collections.Generic
open System.Threading.Tasks
open Microsoft.Owin
open WebSharper
open WebSharper.Sitelets
module M = WebSharper.Core.Metadata

type DepG = WebSharper.Core.DependencyGraph.Graph

/// Options to initialize a sitelet server with IAppBuilder.UseCustomSitelet.
[<Sealed>]
type Options =
    /// Creates empty sitelet server options.
    [<System.Obsolete "Use WebSharperOptions.">]
    static member Create : unit -> Options

    /// Creates sitelet server options with the given WebSharper metadata.
    [<System.Obsolete "Use WebSharperOptions.">]
    static member Create : M.Info * DepG -> Options

    /// Creates sitelet server options using webRoot as the root folder and
    /// loading WebSharper metadata from assemblies in binDirectory.
    /// If binDirectory is not specified, the folder containing WebSharper.Owin.dll is used.
    [<System.Obsolete "Use WebSharperOptions.">]
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

    /// Sets what to do when the WebSharper sitelet or remote function throws an exception.
    /// The first parameter is equal to this.Debug.
    member WithOnException : (bool -> IOwinResponse -> exn -> Task) -> Options

    /// The default action taken when an exception is uncaught from a sitelet or remote function:
    /// set StatusCode to 500 and write the stack trace if Debug is true,
    /// or "Internal Server Error" otherwise.
    static member DefaultOnException : bool -> IOwinResponse -> exn -> Task

type Env = IDictionary<string, obj>
type AppFunc = Func<Env, Task>
type MidFunc = Func<AppFunc, AppFunc>

/// The keys that WebSharper sets in the Owin environment.
module EnvKey =

    /// The ASP.NET HttpContext, if any.
    /// Type: System.Web.HttpContextBase.
    val HttpContext : string

    module WebSharper =

        /// The parsed Sitelets request.
        /// Type: WebSharper.Sitelets.Http.Request.
        val Request : string

        /// The WebSharper context.
        /// Type: WebSharper.Web.IContext.
        /// If the request was recognized by a sitelet, then the concrete type is
        /// WebSharper.Sitelets.Context<'Endpoint>.
        val Context : string

        /// The logged-in user principal, if any.
        /// Type: option<System.Security.Principal.GenericPrincipal>.
        val User : string

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
    /// If binDirectory is not specified, the folder containing WebSharper.Owin.dll is used.
    static member UseRemoting : webRoot: string * ?binDirectory: string -> (AppFunc -> RemotingMiddleware)
    /// Runs the WebSharper Remoting service, allowing WebSharper-compiled
    /// client-side code to invoke [<Rpc>]-annotated server-side functions.
    /// WebSharper metadata is loaded from binDirectory.
    /// If binDirectory is not specified, the folder containing WebSharper.Owin.dll is used.
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
    /// If binDirectory is not specified, the folder containing WebSharper.Owin.dll is used.
    static member UseDiscoveredSitelet : webRoot: string * ?binDirectory: string -> (AppFunc -> SiteletMiddleware<obj>)
    /// Inspects the binDirectory folder, looking for an assembly that contains
    /// a WebSharper Sitelet, and runs this Sitelet with webRoot as the root folder.
    /// Also runs the Remoting service using metadata discovered from binDirectory.
    /// If binDirectory is not specified, the folder containing WebSharper.Owin.dll is used.
    static member AsMidFunc : webRoot: string * ?binDirectory: string -> MidFunc

/// Options to initialize WebSharper.
type WebSharperOptions<'T when 'T: equality> =
    new : unit -> WebSharperOptions<'T>

    /// Get or set the web application's root directory.
    /// Default: the current working directory.
    member ServerRootDirectory : string with get, set

    /// Get or set the web application's binary directory.
    /// Default: the folder containing WebSharper.Owin.dll.
    member BinDirectory : string with get, set

    /// Whether to serve WebSharper RPC functions.
    /// Default: true.
    member UseRemoting : bool with get, set

    /// The URL prefix under which sitelets are served.
    /// Default: empty.
    member UrlPrefix : string with get, set

    /// Whether to serve JavaScript and CSS in debug mode (ie. uncompressed).
    /// Default: false.
    member Debug : bool with get, set

    /// The metadata to use for client code.
    /// If None, it will be loaded from BinDirectory.
    /// Default: None.
    member MetadataAndGraph : option<M.Info * DepG> with get, set

    /// The sitelet to serve.
    /// Default: None.
    member Sitelet : option<Sitelet<'T>> with get, set

    /// If true and this.Sitelet is None, search the binaries folder for a sitelet assembly.
    member DiscoverSitelet : bool with get, set

    /// What to do when the WebSharper sitelet or remote function throws an exception.
    /// The first parameter is equal to this.Debug.
    /// Default: set StatusCode to 500 and write the stack trace if Debug is true,
    /// or "Internal Server Error" otherwise.
    member OnException : (bool -> IOwinResponse -> exn -> Task) with get, set

    /// The default action taken when an exception is uncaught from a sitelet or remote function:
    /// set StatusCode to 500 and write the stack trace if Debug is true,
    /// or "Internal Server Error" otherwise.
    static member DefaultOnException : bool -> IOwinResponse -> exn -> Task

    /// Set the sitelet to serve.
    member WithSitelet : Sitelet<'T> -> WebSharperOptions<'T>

    /// Run WebSharper as an Owin middleware function.
    member AsMidFunc : unit -> MidFunc

[<AutoOpen>]
module Extensions =

    open global.Owin

    type IAppBuilder with
        /// Inspects the binDirectory folder, looking for an assembly that contains
        /// a WebSharper Sitelet, and runs this Sitelet with webRoot as the root folder.
        /// Also runs the Remoting service using metadata discovered from binDirectory.
        /// If binDirectory is not specified, the folder containing WebSharper.Owin.dll is used.
        member UseDiscoveredSitelet : webRoot: string * ?binDirectory: string -> IAppBuilder

        /// Runs the provided Sitelet with webRoot as the root folder, using
        /// WebSharper metadata loaded from assemblies located in binDirectory.
        /// Also runs the Remoting service using metadata discovered from binDirectory.
        /// If binDirectory is not specified, the folder containing WebSharper.Owin.dll is used.
        member UseSitelet : webRoot: string * Sitelet<'T> * ?binDirectory: string -> IAppBuilder

        /// Runs the provided Sitelet with the provided options.
        [<System.Obsolete "Use UseWebSharper(options)">]
        member UseCustomSitelet : Options * Sitelet<'T> -> IAppBuilder

        /// Runs the WebSharper Remoting service, allowing WebSharper-compiled
        /// client-side code to invoke [<Rpc>]-annotated server-side functions.
        /// Note that the Remoting service is automatically run by the
        /// methods UseDiscoveredSitelet and UseSitelet, as well as
        /// UseCustomSitelet if options.RunRemoting is set to true.
        /// WebSharper metadata is loaded from binDirectory.
        /// If binDirectory is not specified, the folder containing WebSharper.Owin.dll is used.
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

        member UseWebSharper : options:WebSharperOptions<'T> -> IAppBuilder
