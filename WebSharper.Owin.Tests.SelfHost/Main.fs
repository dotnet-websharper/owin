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

    let run rootDirectory =
        let url = "http://localhost:9000/"
        try
            use server = WebApp.Start(url, fun appB ->
                appB.UseStaticFiles(
                        StaticFileOptions(
                            FileSystem = PhysicalFileSystem(rootDirectory)))
                    .UseWebSharper(
                        WebSharperOptions(
                            ServerRootDirectory = rootDirectory,
                            DiscoverSitelet = true,
                            Debug = true,
                            UseRemoting = false))
                |> ignore)
            printfn "Serving %s" url
            stdin.ReadLine() |> ignore
            0
        with e ->
            eprintfn "Error starting website:\n%A" e
            1

    [<EntryPoint>]
    let main = function
        | [||] -> run (Directory.GetCurrentDirectory())
        | [| root |] -> run root
        | _ ->
            eprintfn "Usage: OwinSample [ROOT_DIR]"; exit 1
            0
