namespace IntelliFactory.WebSharper.Owin.Tests.Sitelet

open IntelliFactory.WebSharper

module Rpc =

    [<Rpc>]
    let GetValue() = async { return "Server-side value" }

module Client =

    open IntelliFactory.WebSharper.Html

    [<JavaScript>]
    let test = 42

    type Control() =
        inherit Web.Control()

        [<JavaScript>]
        override this.Body =
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
            ] :> _

module Server =

    open IntelliFactory.WebSharper.Sitelets
    open IntelliFactory.Html

    type Action =
        | Index
        | Article of articleId: int
        | Upload

    let IndexPage =
        Content.PageContent <| fun ctx ->
            { Page.Default with
                Body =
                    [
                        H1 [Text "Welcome to my site!"]
                        UL [
                            LI [A [HRef (ctx.Link (Article 1))] -< [Text "Article 1"]]
                            LI [A [HRef (ctx.Link (Article 2))] -< [Text "Article 2"]]
                        ]
                        H2 [Text "Client-side control:"]
                        Div [new Client.Control()]
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
                    ] }

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

    let Sitelet =
        Sitelet.Sum [
            Sitelet.Content "/" Index IndexPage
            Sitelet.Infer <| function
                | Index -> IndexPage
                | Article n -> ArticlePage n
                | Upload -> UploadPage
        ]
