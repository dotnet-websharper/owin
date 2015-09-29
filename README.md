# Overview

This library allows you to run a [WebSharper](http://websharper.com)
Sitelet application through an [OWIN](http://owin.org/) interface,
version 1.0. In the terminology of [the OWIN
specification](http://owin.org/spec/spec/owin-1.0.0.html), WebSharper
is a Web Framework and WebSharper.Owin is an adapter layer for it.

After adding the reference to the project all the classes can be found
under the `WebSharper.Owin` module.

# Usage

WebSharper.Owin provides its functionality through several extension
methods on the `IAppBuilder` type. They are the following:

```fsharp
UseDiscoveredSitelet : webRoot: string -> IAppBuilder
```

Inspects the `webRoot` folder, looking for an assembly in the `bin`
subfolder that contains a WebSharper Sitelet, and runs this Sitelet
with `webRoot` as the root folder.

```fsharp
UseSitelet : webRoot: string * Sitelet<'T> -> IAppBuilder
```

Runs the provided Sitelet with `webRoot` as the root folder, using
WebSharper metadata loaded from assemblies located in the `bin`
subfolder of `webRoot`.

```fsharp
UseCustomSitelet : Options * Sitelet<'T> -> IAppBuilder
```

Runs the provided Sitelet. Allows a more customized setup than the
previous methods, for example running a Sitelet whose code isn't
located in the `bin` subfolder of the root folder, or running the
Sitelet with a URL prefix.

```fsharp
UseWebSharperRemoting : webRoot: string -> IAppBuilder
```

Runs the WebSharper Remoting service, allowing WebSharper-compiled
client-side code to invoke `[<Rpc>]`-annotated server-side functions.
Note that the Remoting service is automatically run by the above
methods `UseDiscoveredSitelet` and `UseSitelet`, as well as
`UseCustomSitelet` if `options.RunRemoting` is set to `true`.

# Notes

This library does not take care of serving the files extracted from
WebSharper assemblies, such as the generated JavaScript files, from
the file system. You need to use a static files middleware. The
example self-hosted Sitelet application uses a middleware available
from NuGet as `Microsoft.Owin.StaticFiles`, as follows:

```fsharp
open global.Owin
open Microsoft.Owin.StaticFiles
open Microsoft.Owin.FileSystems
open WebSharper.Owin

let RunSitelet (appB: IAppBuilder) mySitelet rootFolder =
    appB.UseStaticFiles(
        StaticFileOptions(
            FileSystem = PhysicalFileSystem(rootFolder)))
        .UseSitelet(rootFolder, mySitelet)
```
