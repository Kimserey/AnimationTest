namespace AnimationUINext

open WebSharper
open WebSharper.JavaScript
open WebSharper.JQuery
open WebSharper.UI.Next
open WebSharper.UI.Next.Html
open WebSharper.UI.Next.Client

[<JavaScript>]
module SimpleAnimation =

    let anim = Anim.Simple Interpolation.Double Easing.CubicInOut 500.    

    let trans = 
        Trans.Trivial()
        |> Trans.Enter (fun v -> anim 0. v)
        |> Trans.Exit  (fun v -> anim v 0.) 

    let rvShow = Var.Create true

    let main =
        [ Doc.Button 
          <| "Toggle" 
          <| [ attr.style "display: block;" ] 
          <| fun () -> rvShow.Value <- not rvShow.Value 
          :> Doc
          
          rvShow.View
          |> View.Map (fun show ->
            if show then
                h1Attr [ Attr.AnimatedStyle
                         <| "transform"
                         <| trans
                         <| View.Const 5.
                         <| sprintf "translate(%fem)"
                        
                         Attr.AnimatedStyle
                         <| "opacity"
                         <| trans
                         <| View.Const 1.
                         <| sprintf "%f" ]
                        
                    [ text "Hello world." ] :> Doc
            else
                Doc.Empty)
          |> Doc.EmbedView ]
        |> Doc.Concat
        |> Doc.RunBeforeById "main"

