module WebSharper.Owin.Tests.SelfHost

open System
open System.IO
open WebSharper

module SelfHostedServer =

    open global.Owin
    open Microsoft.Owin.Hosting
    open Microsoft.Owin.StaticFiles
    open Microsoft.Owin.FileSystems
    open WebSharper.Owin
    open WebSharper.Owin.Tests.Sitelet

    [<EntryPoint>]
    let Main args =
        if args.Length = 1 then
            let url = "http://localhost:9000/"
            let workingDirectory = Path.Combine(Directory.GetCurrentDirectory(), args.[0])
            try
                use server = WebApp.Start(url, fun appB ->
                    appB.UseStaticFiles(
                            StaticFileOptions(
                                FileSystem = PhysicalFileSystem(workingDirectory)))
                        .UseDiscoveredSitelet(workingDirectory)
                    |> ignore)
                stdout.WriteLine("Serving {0}", url)
                stdin.ReadLine() |> ignore
                0
            with e ->
                eprintfn "Error starting website:\n%s" e.Message
                1
        else
            eprintfn "Usage: OwinSample WORKING_DIRECTORY"
            1
