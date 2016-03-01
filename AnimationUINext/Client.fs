namespace AnimationUINext

open WebSharper
open WebSharper.JavaScript
open WebSharper.JQuery
open WebSharper.UI.Next
open WebSharper.UI.Next.Html
open WebSharper.UI.Next.Client

[<JavaScript>]
module Client =

    let (<*>) f m = m |> View.Apply f
    
    module Colors =
        let green = "#689F38"
        let red   = "#ED3B3B"
        let blue  = "#039BE5"
        let grey  = "#658092"

    let transition initial =
        let anim time = Anim.Simple Interpolation.Double Easing.CubicInOut time
        Trans.Trivial()
        |> Trans.Change (anim 350.)
        |> Trans.Enter (fun v -> anim 200. initial v)
        |> Trans.Exit (fun v -> anim 200. v initial)

    type State = {
        Opacity: double
        Position: Position
        IsSelected: bool
    } with
        static member hide x =
            { x with Opacity = 0. }
        static member show x =
            { x with Opacity = 1. }
        static member moveUp x =
            { x with Position = { x.Position with Top = x.Position.Top - 1. } }
        static member moveUpN n x =
            { x with Position = { x.Position with Top = x.Position.Top - (float n * 1.) } }
        static member moveDown x =
            { x with Position = { x.Position with Top = x.Position.Top + 1. } }
        static member moveDownN n x =
            { x with Position = { x.Position with Top = x.Position.Top + (float n * 1.) } }
        static member moveLeft x =
            { x with Position = { x.Position with Left = x.Position.Left - 1. } }
        static member moveRight x =
            { x with Position = { x.Position with Left = x.Position.Left + 1. } }
        static member select x =
            { x with IsSelected = true }
        static member unselect x =
            { x with IsSelected = false }
        static member create() =
            { Opacity = 1.; Position =  { Top = 0.; Left = 0. }; IsSelected = false }
    and Position = {
        Top: double
        Left: double
    }

    type Button = {
        Title: string
        Icon: string
        HexColor: string
        OnClick: unit -> unit
        SelectedClass: string
        State: Var<State>
    } with
        static member create() = { Title = ""; Icon = ""; HexColor = ""; OnClick = ignore; State = Var.Create (State.create()); SelectedClass = "" }
        static member setTitle title x = { x with Title = title }
        static member setIcon icon x = { x with Icon = icon }
        static member setColor color x = { x with HexColor = color } 
        static member setState state x = { x with State = state }
        static member onClick action x = { x with OnClick = action }
        static member setSelectedClass cls x = { x with SelectedClass = cls }
        static member render x =
            let left (state: State) = state.Position.Left * 300.
            let top (state: State)  = state.Position.Top * 48.
            let opacity (state: State) = state.Opacity
            let rvHover = Var.Create false

            divAttr [ attr.classDyn ((rvHover.View, x.State.View) 
                                     ||> View.Map2 (fun h s -> if h || s.IsSelected then "menu-button " + x.SelectedClass else "menu-button"))
                      on.click (fun _ _ -> x.OnClick())
                      on.mouseOver(fun _ _ -> Var.Set rvHover true)
                      on.mouseOut (fun _ _ -> Var.Set rvHover false)
                      Attr.AnimatedStyle "opacity" (transition 1.) (View.Map opacity x.State.View) (sprintf "%f")
                      Attr.AnimatedStyle "left"    (transition 0.) (View.Map left    x.State.View) (sprintf "%fpx")
                      Attr.AnimatedStyle "top"     (transition 0.) (View.Map top     x.State.View) (sprintf "%fpx") ]
                    [ divAttr [ attr.style (sprintf "background-color:%s;" x.HexColor)
                                attr.``class`` "menu-button-icon" ]
                              [ iAttr [ attr.``class`` (sprintf "fa %s fa-2x fa-fw" x.Icon) ] [] ]
                      divAttr [ attr.``class`` "menu-button-text" ] 
                              [ text x.Title ] ]

    let mobileBtn =
        Button.create()
        |> Button.setIcon "fa-mobile" 
        |> Button.setTitle "Mobile phones" 
        |> Button.setColor Colors.green
        |> Button.setSelectedClass "menu-button-is-selected-green"

    let tabletBtn =
        Button.create()
        |> Button.setIcon "fa-tablet"
        |> Button.setTitle "Tablets"
        |> Button.setColor Colors.red
        |> Button.setSelectedClass "menu-button-is-selected-red"

    let laptopBtn =
        Button.create()
        |> Button.setIcon "fa-laptop"
        |> Button.setTitle "Laptops"
        |> Button.setColor Colors.blue
        |> Button.setSelectedClass "menu-button-is-selected-blue"

    let accessoriesBtn =
        Button.create()
        |> Button.setIcon "fa-keyboard-o"
        |> Button.setTitle "Accessories"
        |> Button.setColor Colors.grey
        |> Button.setSelectedClass "menu-button-is-selected-grey"

    module Selection =
        let mobile() =
            if not mobileBtn.State.Value.IsSelected then
                Var.Set mobileBtn.State       (mobileBtn.State.Value |> State.select)
                Var.Set tabletBtn.State       (tabletBtn.State.Value |> State.moveLeft |> State.hide)
                Var.Set laptopBtn.State       (laptopBtn.State.Value |> State.moveLeft |> State.hide)
                Var.Set accessoriesBtn.State  (accessoriesBtn.State.Value |> State.moveLeft |> State.hide)
        
        let tablet() =
            if not tabletBtn.State.Value.IsSelected then
                Var.Set mobileBtn.State       (mobileBtn.State.Value |> State.moveLeft |> State.hide)
                Var.Set tabletBtn.State       (tabletBtn.State.Value |> State.select |> State.moveUp)
                Var.Set laptopBtn.State       (laptopBtn.State.Value |> State.moveLeft |> State.hide)
                Var.Set accessoriesBtn.State  (accessoriesBtn.State.Value |> State.moveLeft |> State.hide)

        let laptop() =
            if not laptopBtn.State.Value.IsSelected then
                Var.Set mobileBtn.State       (mobileBtn.State.Value |> State.moveLeft |> State.hide)
                Var.Set tabletBtn.State       (tabletBtn.State.Value |> State.moveLeft |> State.hide)
                Var.Set laptopBtn.State       (laptopBtn.State.Value |> State.select |> State.moveUpN 2)
                Var.Set accessoriesBtn.State  (accessoriesBtn.State.Value |> State.moveLeft |> State.hide)

        let accessories() =
            if not accessoriesBtn.State.Value.IsSelected then
                Var.Set mobileBtn.State       (mobileBtn.State.Value |> State.moveLeft |> State.hide)
                Var.Set tabletBtn.State       (tabletBtn.State.Value |> State.moveLeft |> State.hide)
                Var.Set laptopBtn.State       (laptopBtn.State.Value |> State.moveLeft |> State.hide)
                Var.Set accessoriesBtn.State  (accessoriesBtn.State.Value |> State.select |> State.moveUpN 3)
    
    module Content =
        let mobile =
            divAttr [ attr.``class`` "menu-content"
                      Attr.AnimatedStyle "opacity" (transition 0.) (View.Const 1.) (sprintf "%f") ] 
                    [ divAttr [ attr.``class`` "menu-content-back"
                                on.click (fun _ _ ->
                                    Var.Set mobileBtn.State       (mobileBtn.State.Value |> State.unselect)
                                    Var.Set tabletBtn.State       (tabletBtn.State.Value |> State.moveRight |> State.show)
                                    Var.Set laptopBtn.State       (laptopBtn.State.Value |> State.moveRight |> State.show)
                                    Var.Set accessoriesBtn.State  (accessoriesBtn.State.Value |> State.moveRight |> State.show)) ]
                              [ text "‹" ]
                      divAttr [ attr.``class`` "menu-content-links" ] 
                              [ div [ text "Sim free" ]
                                div [ text "Iphone" ]
                                div [ text "Samsung" ] ] ]

        let tablet =
            divAttr [ attr.``class`` "menu-content"
                      Attr.AnimatedStyle "opacity" (transition 0.) (View.Const 1.) (sprintf "%f") ] 
                    [ divAttr [ attr.``class`` "menu-content-back"
                                on.click (fun _ _ ->
                                    Var.Set mobileBtn.State       (mobileBtn.State.Value |> State.moveRight |> State.show)
                                    Var.Set tabletBtn.State       (tabletBtn.State.Value |> State.unselect  |> State.moveDown)
                                    Var.Set laptopBtn.State       (laptopBtn.State.Value |> State.moveRight |> State.show)
                                    Var.Set accessoriesBtn.State  (accessoriesBtn.State.Value |> State.moveRight |> State.show)) ]
                          [ text "‹" ]
                      divAttr [ attr.``class`` "menu-content-links" ] 
                              [ div [ text "Ipad" ]
                                div [ text "Samsung Galaxy tab" ]
                                div [ text "Android" ] ] ]

        let laptop =
            divAttr [ attr.``class`` "menu-content"
                      Attr.AnimatedStyle "opacity" (transition 0.) (View.Const 1.) (sprintf "%f") ] 
                    [ divAttr [ attr.``class`` "menu-content-back"
                                on.click (fun _ _ ->
                                    Var.Set mobileBtn.State       (mobileBtn.State.Value |> State.moveRight |> State.show)
                                    Var.Set tabletBtn.State       (tabletBtn.State.Value |> State.moveRight |> State.show)
                                    Var.Set laptopBtn.State       (laptopBtn.State.Value |> State.unselect  |> State.moveDownN 2)
                                    Var.Set accessoriesBtn.State  (accessoriesBtn.State.Value |> State.moveRight |> State.show)) ]
                              [ text "‹" ]
                      divAttr [ attr.``class`` "menu-content-links" ] 
                              [ div [ text "Macbook pro" ]
                                div [ text "Macbook pro retina" ]
                                div [ text "Lenovo" ] ] ]

        let accessories =
            divAttr [ attr.``class`` "menu-content"
                      Attr.AnimatedStyle "opacity" (transition 0.) (View.Const 1.) (sprintf "%f") ] 
                    [ divAttr [ attr.``class`` "menu-content-back"
                                on.click (fun _ _ ->
                                    Var.Set mobileBtn.State       (mobileBtn.State.Value |> State.moveRight |> State.show)
                                    Var.Set tabletBtn.State       (tabletBtn.State.Value |> State.moveRight |> State.show)
                                    Var.Set laptopBtn.State       (laptopBtn.State.Value |> State.moveRight |> State.show)
                                    Var.Set accessoriesBtn.State  (accessoriesBtn.State.Value |> State.unselect  |> State.moveDownN 3)) ]
                              [ text "‹" ]
                      divAttr [ attr.``class`` "menu-content-links" ] 
                              [ div [ text "Keyboards" ]
                                div [ text "Mice" ]
                                div [ text "Headphones" ] ] ]

                                    
    let main =
        divAttr [ attr.``class`` "menu" ]
                [ mobileBtn
                  |> Button.onClick Selection.mobile
                  |> Button.render

                  tabletBtn
                  |> Button.onClick Selection.tablet
                  |> Button.render

                  laptopBtn
                  |> Button.onClick Selection.laptop
                  |> Button.render

                  accessoriesBtn
                  |> Button.onClick Selection.accessories
                  |> Button.render 

                  View.Const(fun (sm: State) (st: State) (sl: State) (sa:State) -> 
                    if sm.IsSelected      then Content.mobile :> Doc
                    else if st.IsSelected then Content.tablet :> Doc
                    else if sl.IsSelected then Content.laptop :> Doc
                    else if sa.IsSelected then Content.accessories :> Doc
                    else Doc.Empty)
                  <*> mobileBtn.State.View
                  <*> tabletBtn.State.View
                  <*> laptopBtn.State.View
                  <*> accessoriesBtn.State.View
                  |> Doc.EmbedView ]
        |> Doc.RunAfterById "main"
