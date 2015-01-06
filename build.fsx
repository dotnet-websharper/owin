#load "tools/includes.fsx"
open IntelliFactory.Build

let bt =
    BuildTool().PackageId("WebSharper.Owin", "3.0-alpha")

let multipartParser =
    bt.MSBuild("HttpMultipartParser/HttpMultipartParser.csproj")
        .Configuration("Release")

let main =
    bt.WebSharper.Library("IntelliFactory.WebSharper.Owin")
        .SourcesFromProject()
        .References(fun r ->
            [
                r.File("HttpMultipartParser/bin/Release/HttpMultipartParser.dll")
                r.NuGet("Owin").Reference()
                r.NuGet("Microsoft.Owin").Reference()
                r.Assembly("System.Configuration")
                r.Assembly "System.Web"
            ])

let testSitelet =
    bt.WebSharper.SiteletWebsite("IntelliFactory.WebSharper.Owin.Tests.Sitelet")
        .SourcesFromProject()
        .References(fun r ->
            [
                r.Project(main)
            ])

let testHost =
    bt.FSharp.ConsoleExecutable("IntelliFactory.WebSharper.Owin.Tests.SelfHost")
        .SourcesFromProject()
        .References(fun r ->
            [
                r.Project(main)
                r.Project(testSitelet)
                r.NuGet("WebSharper").At(
                    [
                        "lib/net40/IntelliFactory.WebSharper.Sitelets.dll"
                        "lib/net40/IntelliFactory.WebSharper.Core.dll"
                    ]).Reference()
                r.NuGet("Microsoft.Owin").Reference()
                r.NuGet("Microsoft.Owin.Diagnostics").Reference()
                r.NuGet("Microsoft.Owin.FileSystems").Reference()
                r.NuGet("Microsoft.Owin.Host.HttpListener").Reference()
                r.NuGet("Microsoft.Owin.Hosting").Reference()
                r.NuGet("Microsoft.Owin.SelfHost").Reference()
                r.NuGet("Microsoft.Owin.StaticFiles").Reference()
                r.NuGet("Mono.Cecil").Reference()
            ])

bt.Solution [
    multipartParser
    main
    testSitelet
    testHost

    bt.NuGet.CreatePackage()
        .Configure(fun c ->
            { c with
                Title = Some "WebSharper.Owin-1.0"
                LicenseUrl = Some "http://websharper.com/licensing"
                ProjectUrl = Some "https://github.com/intellifactory/websharper.owin"
                Description = "WebSharper Sitelets module for Owin 1.0"
                RequiresLicenseAcceptance = true })
        .Add(main)
        .AddFile(
            "HttpMultipartParser/bin/Release/HttpMultipartParser.dll",
            "lib/net45/HttpMultipartParser.dll")

]
|> bt.Dispatch
