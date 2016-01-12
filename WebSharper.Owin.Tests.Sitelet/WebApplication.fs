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

module Client =

    open WebSharper.Html.Client
    open WebSharper.JavaScript

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

    type LogoutControl(loggedInAs) =
        inherit Web.Control()

        [<JavaScript>]
        override this.Body =
            Div [
                Span [Text ("Logged in as " + loggedInAs)]
                Button [Text "Log out"]
                |>! OnClick (fun _ _ ->
                    async {
                        do! Rpc.Logout()
                        return JS.Window.Location.Reload()
                    } |> Async.Start)
            ]
            :> _

    type LoginControl() =
        inherit Web.Control()

        [<JavaScript>]
        override this.Body =
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
            :> _

module Server =

    open WebSharper.Sitelets
    open WebSharper.Html.Server

    type Action =
        | Index
        | Article of articleId: int
        | Upload
        | [<Method "POST"; FormData("x", "y")>] Post of x: int * y: string

    let Header (ctx: Context<_>) =
        async {
            let! loggedIn = ctx.UserSession.GetLoggedInUser()
            match loggedIn with
            | Some u -> return Div [new Client.LogoutControl(u)]
            | None -> return Div [new Client.LoginControl()]
        }

    let IndexPage ctx =
        async {
            let! header = Header ctx
            return! Content.Page(
                Body =
                    [
                        header
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
                        H2 [Text "Form to test application/x-www-urlencoded management"]
                        Form [
                            Attr.Action (ctx.Link (Post (0, "")))
                            Method "post"
                            EncType "application/x-www-urlencoded"
                        ] -< [
                            Div [Label [Text "X: "; Input [Name "x"; Type "number"]]]
                            Div [Label [Text "Y: "; Input [Name "y"]]]
                            Div [Input [Type "submit"]]
                        ]
                    ]
            )
        }

    let ArticlePage articleId ctx =
        Content.Page(
            Body =
                [
                    H1 [Text ("Article " + string articleId)]
                    P [Text "Hello world! Now I just need to add some content..."]
                    P [A [HRef (ctx.Link Index)] -< [Text "Back to home"]]
                ]
        )

    let UploadPage ctx =
        Content.Page(
            Body =
                match Array.ofSeq ctx.Request.Files with
                | [||] ->
                    [
                        Text "No file uploaded. "
                        A [HRef (ctx.Link Index)] -< [Text "Back to home"]
                    ]
                | files ->
                    let name = defaultArg ctx.Request.Post.["name"] "(not provided)"
                    let body =
                        files |> Array.map (fun f ->
                            let text = sprintf "Uploaded file: '%s', %i B" f.FileName f.ContentLength
                            printfn "%s" text
                            use r = new System.IO.StreamReader(f.InputStream)
                            printfn "%s" (r.ReadToEnd())
                            P [Text text])
                    [
                        yield P [Text ("Your name is " + name)]
                        yield! body
                        yield A [HRef (ctx.Link Index)] -< [Text "Back to home"]
                    ]
        )

    [<Website>]
    let Sitelet =
        Sitelet.Sum [
            Sitelet.Content "/" Index IndexPage
            Sitelet.Infer <| fun ctx -> function
                | Index -> IndexPage ctx
                | Article n -> ArticlePage n ctx
                | Upload -> UploadPage ctx
                | Post (x, y) -> ArticlePage x ctx
        ]
