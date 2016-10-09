System.Environment.SetEnvironmentVariable("INTELLIFACTORY", null) // Don't sign
#load "tools/includes.fsx"
open IntelliFactory.Build

let bt =
    BuildTool().PackageId("Zafir.Owin")
        .VersionFrom("Zafir")
        .WithFSharpVersion(FSharpVersion.FSharp31)
        .WithFramework(fun fw -> fw.Net45)

let multipartParser =
    bt.MSBuild("HttpMultipartParser/HttpMultipartParser.csproj")
        .Configuration("Release")
        .GeneratedAssemblyFiles(
            [
                __SOURCE_DIRECTORY__ + "/HttpMultipartParser/bin/Release/HttpMultipartParser.dll"
            ]
        )

let main =
    bt.Zafir.Library("WebSharper.Owin")
        .SourcesFromProject()
        .References(fun r ->
            [
                r.Project(multipartParser)
                r.NuGet("Owin").ForceFoundVersion().Reference()
                r.NuGet("Arachne.Http").ForceFoundVersion().Reference()
                r.NuGet("Arachne.Http.State").ForceFoundVersion().Reference()
                r.NuGet("Arachne.Uri").ForceFoundVersion().Reference()
                r.Assembly("System.Configuration")
                r.Assembly "System.Web"
            ])

let testSitelet =
    bt.Zafir.SiteletWebsite("WebSharper.Owin.Tests.Sitelet")
        .SourcesFromProject()
        .References(fun r ->
            [
                r.Project(multipartParser)
                r.Project(main)
                r.NuGet("Zafir.Html").Latest(true).Reference()
            ])

let testHost =
    bt.Zafir.Executable("WebSharper.Owin.Tests.SelfHost")
        .SourcesFromProject()
        .References(fun r ->
            [
                r.Project(multipartParser)
                r.Project(main)
                r.Project(testSitelet)
                r.NuGet("Arachne").Reference()
                r.NuGet("Microsoft.Owin").Reference()
                r.NuGet("Microsoft.Owin.Diagnostics").Reference()
                r.NuGet("Microsoft.Owin.FileSystems").Reference()
                r.NuGet("Microsoft.Owin.Host.HttpListener").Reference()
                r.NuGet("Microsoft.Owin.Hosting").Reference()
                r.NuGet("Microsoft.Owin.SelfHost").Reference()
                r.NuGet("Microsoft.Owin.StaticFiles").Reference()
                r.NuGet("Mono.Cecil").Reference()
                r.NuGet("Zafir.Html").Latest(true).Reference()
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
