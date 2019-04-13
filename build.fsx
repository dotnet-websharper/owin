#r "paket: groupref build //"
#load "paket-files/wsbuild/github.com/dotnet-websharper/build-script/WebSharper.Fake.fsx"
open WebSharper.Fake

LazyVersionFrom "WebSharper"
|> WSTargets.Default
|> MakeTargets
|> RunTargets
