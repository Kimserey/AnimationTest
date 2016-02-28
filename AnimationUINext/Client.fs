namespace AnimationUINext

open WebSharper
open WebSharper.JavaScript
open WebSharper.JQuery
open WebSharper.UI.Next
open WebSharper.UI.Next.Html
open WebSharper.UI.Next.Client

[<AutoOpen; JavaScript>]
module Animation =
    let anim time = 
        Anim.Simple Interpolation.Double Easing.CubicInOut time

    type Fade = {
        In: bool
        Out: bool
        TimeInMs: double
    } with
        static member fadesIn x ms = { In = true; Out = false; TimeInMs = ms }
        static member fadesOut x ms = { In = false; Out = true; TimeInMs = ms }
        static member fadesInAndOut x ms = { In = true; Out = true; TimeInMs = ms }
        static member create x =
            let anim = anim x.TimeInMs
            let transition =
                Trans.Create anim
                |> (if x.In then Trans.Enter (fun x -> anim 0. x) else id)
                |> (if x.Out then Trans.Exit (fun x -> anim x 0.) else id)
            Attr.AnimatedStyle "opacity" transition (View.Const 1.) string   

    type Slide = {
        Direction: SlideDirection
        Initial: double
        SlideBy: double
        TimeInMs: double 
    }
    with
        static member Default = 
            { Direction = Up
              Initial = 0.
              SlideBy = 0.
              TimeInMs = 0. }
        static member takesDirection  dir x = { x with Direction = dir }
        static member startsAtPixels px x = { x with Initial = px }
        static member slidesByPixels px x = { x with SlideBy = px }
        static member lastsForInMs    ms (x: Slide) = { x with TimeInMs = ms }
        static member create x =
            let (style, animation, final) = 
                match x.Direction with 
                | Up    -> 
                    let value = x.Initial - x.SlideBy
                    "top", anim x.TimeInMs x.Initial value, value

                | Down  -> 
                    let value = x.Initial + x.SlideBy
                    "top", anim x.TimeInMs x.Initial value, value

                | Left  -> 
                    let value = x.Initial - x.SlideBy
                    "left", anim x.TimeInMs x.Initial value, value

                | Right -> 
                    let value = x.Initial + x.SlideBy
                    "left", anim x.TimeInMs x.Initial value, value

            Attr.AnimatedStyle style (Trans.Create (anim x.TimeInMs) 
            |> Trans.Enter (fun _ -> animation)) (View.Const final) (sprintf "%Apx")

    and SlideDirection = Up | Down | Left | Right


[<JavaScript>]
module Client =

    let rvTest = Var.Create 5
    let rvShow = Var.Create true

    let btns =
        [ div [ text "hide"
                Doc.CheckBox [] rvShow :> Doc ]
          Doc.IntInputUnchecked [] rvTest ]
        |> Seq.cast

    /// anim represents the value being animated over the duration
    let anim1 =
        Anim.Simple Interpolation.Double Easing.CubicInOut 750.

    let animation =
        Trans.Trivial()
        |> Trans.Change anim1
        |> Trans.Enter (fun x -> anim1 0. x)
        |> Trans.Exit (fun x -> anim1 x 0.)

    /// a transition is used to describe what to do with the animated value
    let main =
        [ rvShow.View
          |> View.Map (function
            | true -> divAttr [ attr.style "float: left;"
                                Attr.AnimatedStyle "opacity" animation (View.Const 1.) string
                                Attr.AnimatedStyle "transform" animation (View.Map double rvTest.View) (sprintf "translateY(%fem)") ]
                              [ h1Attr [ attr.style "width: 160px;" ] 
                                       [ text "hello world" ] ] :> Doc
            | false -> Doc.Empty)
          |> Doc.EmbedView
          divAttr [ attr.style "float: right;" ] btns :> Doc ]
        |> Doc.Concat
        |> Doc.RunById "main"
