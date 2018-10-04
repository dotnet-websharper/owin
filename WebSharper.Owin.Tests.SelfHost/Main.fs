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
//                        .UseDiscoveredSitelet(workingDirectory)
                        .UseWebSharper(
                            WebSharperOptions(
                                ServerRootDirectory = workingDirectory,
                                DiscoverSitelet = true,
                                OnException = (fun debug resp exn ->
                                    resp.StatusCode <- 500
                                    resp.WriteAsync("[UNCAUGHT EXCEPTION]\r\n" + string exn)
                                )
                            )
                        )
                    |> ignore)
                stdout.WriteLine("Serving {0}", url)
                stdin.ReadLine() |> ignore
                0
            with e ->
                eprintfn "Error starting website:\n%A" e
                1
        else
            eprintfn "Usage: OwinSample WORKING_DIRECTORY"
            1
