namespace WebSharper.Owin.Tests.Sitelet

open WebSharper

module Rpc =

    [<Rpc>]
    let GetValue() = async { return "Server-side value" }

    [<Rpc>]
    let Logout() =
        let ctx = Web.Remoting.GetContext()
        async {
            do! ctx.UserSession.Logout()
            return ()
        }

    [<Rpc>]
    let LoginAs username =
        let ctx = Web.Remoting.GetContext()
        async {
            do! ctx.UserSession.LoginUser username
            return ()
        }

[<JavaScript>]
module Client =

    open WebSharper.Html.Client
    open WebSharper.JavaScript
    
    let test = 42

    let Main() =
        Div [
            P [Text ("Client-side value: " + string test)]
            P [
                Span [Text "Value retrieved from the server: "]
                Span []
                |>! OnAfterRender (fun span ->
                    async {
                        let! x = Rpc.GetValue()
                        span.Text <- x
                    }
                    |> Async.Start
                )
            ]
        ]

    let Logout loggedInAs =
        Div [
            Span [Text ("Logged in as " + loggedInAs)]
            Button [Text "Log out"]
            |>! OnClick (fun _ _ ->
                async {
                    do! Rpc.Logout()
                    return JS.Window.Location.Reload()
                } |> Async.Start)
        ]

    let Login() =
        let input = Input []
        Div [
            input
            Button [Text "Log in"]
            |>! OnClick (fun _ _ ->
                async {
                    do! Rpc.LoginAs input.Value
                    return JS.Window.Location.Reload()
                } |> Async.Start)
        ]

    open WebSharper.ChartJs

    let Chart () =
        JavaScript.Array<string>() |> ignore

        let n = ref 10

        let chart : Chart.LineChart option ref = ref None
        
        let Push (chart : Chart.LineChart) (value : float) =
//        let Push (value : float) =
//            let chart = (!chart).Value
            chart.RemoveData()
            chart.AddData([| float value |], string (!n + 1)) 
            n := !n + 1

        let initialData =
            [| for x in 1 .. !n do
                    yield (string x, Math.Random()) |]

        Div [
            Canvas [
                Width  "1200"
                Height "400"
            ]
            |>! OnAfterRender (fun canvas ->
                let canvas = As<CanvasElement> canvas.Body
                let (labels, dataset) =
                    Array.unzip initialData

                let data =
                    LineChartData(
                        Labels   = labels,
                        Datasets = [| LineChartDataset(Data = dataset) |]
                    )

                Chart.Defaults.ShowTooltips <- false

                let options =
                    LineChartConfiguration(
                        BezierCurve = false,
                        DatasetFill = false
                    )

                chart := Some <| Chart(canvas.GetContext "2d").Line(data, options)
            )

            Div [
                Button [ Text "Push" ]
                |>! OnClick (fun _ _ ->
                    Push (!chart).Value <| Math.Random()
//                    Push <| Math.Random()
                )
            ]
        ] 

module Server =

    open WebSharper.Sitelets
    open WebSharper.Html.Server

    type Action =
        | Index
        | Article of articleId: int
        | Upload

    let Header (ctx: Context<_>) =
        async {
            let! loggedIn = ctx.UserSession.GetLoggedInUser()
            match loggedIn with
            | Some u -> return Div [ClientSide <@ Client.Logout u @>]
            | None -> return Div [ClientSide <@ Client.Login() @>]
        }

    let IndexPage =
        Content.PageContentAsync <| fun ctx -> async {
            let! header = Header ctx
            return {
              Page.Default with
                Body =
                    [
                        header
                        H1 [Text "Welcome to my site!"]
                        UL [
                            LI [A [HRef (ctx.Link (Article 1))] -< [Text "Article 1"]]
                            LI [A [HRef (ctx.Link (Article 2))] -< [Text "Article 2"]]
                        ]
                        H2 [Text "Client-side control:"]
                        Div [ClientSide <@ Client.Main() @>]
                        H2 [Text "Form to test multipart/form-data management"]
                        Form [
                            Attr.Action (ctx.Link Upload)
                            Method "post"
                            EncType "multipart/form-data"
                        ] -< [
                            Div [
                                Label [For "name"] -< [Text "Your name:"]
                                Input [Type "text"; Name "name"]
                            ]
                            Div [Input [Type "file"; Name "thefile"]]
                            Div [Input [Type "submit"]]
                        ]
                        H2 [Text "Charting demo:"]
                        Div [ClientSide <@ Client.Chart() @>]
                    ] }
        }

    let ArticlePage articleId =
        Content.PageContent <| fun ctx ->
            { Page.Default with
                Body =
                    [
                        H1 [Text ("Article " + string articleId)]
                        P [Text "Hello world! Now I just need to add some content..."]
                        P [A [HRef (ctx.Link Index)] -< [Text "Back to home"]]
                    ] }

    let UploadPage =
        Content.PageContent <| fun ctx ->
            match Array.ofSeq ctx.Request.Files with
            | [||] ->
                { Page.Default with
                    Body =
                        [
                            Text "No file uploaded. "
                            A [HRef (ctx.Link Index)] -< [Text "Back to home"]
                        ] }
            | files ->
                let name = defaultArg ctx.Request.Post.["name"] "(not provided)"
                let body =
                    files |> Array.map (fun f ->
                        let text = sprintf "Uploaded file: '%s', %i B" f.FileName f.ContentLength
                        printfn "%s" text
                        use r = new System.IO.StreamReader(f.InputStream)
                        printfn "%s" (r.ReadToEnd())
                        P [Text text])
                { Page.Default with
                    Body =
                        [
                            yield P [Text ("Your name is " + name)]
                            yield! body
                            yield A [HRef (ctx.Link Index)] -< [Text "Back to home"]
                        ] }

    [<Website>]
    let Sitelet =
        Sitelet.Sum [
            Sitelet.Content "/" Index IndexPage
            Sitelet.Infer <| function
                | Index -> IndexPage
                | Article n -> ArticlePage n
                | Upload -> UploadPage
        ]
