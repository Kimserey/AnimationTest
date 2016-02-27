namespace AnimationUINext

open WebSharper
open WebSharper.JavaScript
open WebSharper.JQuery
open WebSharper.UI.Next
open WebSharper.UI.Next.Html
open WebSharper.UI.Next.Client

[<AutoOpen; JavaScript>]
module Animation =
    let private anim time = 
        Anim.Simple Interpolation.Double Easing.CubicInOut time

    type Fade = {
        In: bool
        Out: bool
        TimeInMs: double
    } with
        static member FadeIn ms = { In = true; Out = false; TimeInMs = ms }
        static member FadeOut ms = { In = false; Out = true; TimeInMs = ms }
        static member FadeInAndOut ms = { In = true; Out = true; TimeInMs = ms }
    
    let fade options =
        let anim =
            anim options.TimeInMs
        let transition =
            Trans.Create anim
            |> (if options.In then Trans.Enter (fun _ -> anim 0. 1.) else id)
            |> (if options.Out then Trans.Exit (fun _ -> anim 1. 0.) else id)
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
        static member setDirection  dir x = { x with Direction = dir }
        static member startsAtPixels px x = { x with Initial = px }
        static member slidesByPixels px x = { x with SlideBy = px }
        static member lastForInMs    ms (x: Slide) = { x with TimeInMs = ms }
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

            Attr.AnimatedStyle style (Trans.Create (anim x.TimeInMs) |> Trans.Enter (fun _ -> animation)) (View.Const final) (sprintf "%Apx")
    and SlideDirection = Up | Down | Left | Right


[<JavaScript>]
module Client =

    let Main =
        h1Attr [ attr.style "position: absolute;"
                 Slide.Default
                 |> Slide.setDirection Down
                 |> Slide.lastForInMs 1000.
                 |> Slide.slidesByPixels 100.
                 |> Slide.create  ] [ text "hello world" ]
        |> Doc.RunById "main"
