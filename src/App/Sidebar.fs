module Sidebar

open Sutil
open Sutil.Attr
open SutilExt


let initiallyOpenedTitles = ["Global Settings"; "Intro"; "Shortcuts"] |> Set.ofList


[<RequireQualifiedAccess>]
type Msg =
  | ToggleAll
  | Toggle of string

type Model =
  {
    Expanded: Set<string>
  }
  static member isExpanded title (m: Model) =
    m.Expanded.Contains title

let init () =
  {
    Expanded = initiallyOpenedTitles
  }, Cmd.batch [ Hotkeys.Cmd.bindHotkey "ctrl+b" Msg.ToggleAll ]

let update msg (model: Model) =
  match msg with
  | Msg.Toggle title ->
      DOM.rafu (fun _ -> DOM.dispatchSimple Browser.Dom.window "resize" )
      let newExpanded =
        if model |> Model.isExpanded title then
          model.Expanded.Remove title
        else
          model.Expanded.Add title
      { model with Expanded = newExpanded }
      , Cmd.none

  | Msg.ToggleAll ->
      DOM.rafu (fun _ -> DOM.dispatchSimple Browser.Dom.window "resize" )
      let newExpanded = if model.Expanded.IsEmpty then initiallyOpenedTitles else Set.empty
      { model with Expanded = newExpanded }
      , Cmd.none


module Subelements =
  let sideBarPanel dispatch title content (store: IStore<Model>) =
    let isExpanded = store .> Model.isExpanded title
    let toggleExpandedClass = Bind.toggleClass(isExpanded, "wm-expanded", "wm-collapsed")
    Html.div [
      Attr.className "sidebar-panel"

      Html.div [
        Attr.className "sidebar-panel-header"
        toggleExpandedClass
        Html.div [
          Attr.style "display: flex; flex-grow: 1"
          Html.button [
            Attr.className "sidebar-panel-header-caption"
            onClick (fun _ -> Msg.Toggle title |> dispatch) []
            Html.div [
              Bind.toggleClass(isExpanded, "codicon codicon-chevron-down", "codicon codicon-chevron-right")
            ]
            Html.div [
              text title
            ]
          ]
        ]
        Html.button [
          Attr.className "sidebar-panel-header-options"
          onClick (fun _ -> Browser.Dom.console.log("gear")) [StopPropagation]
          Html.div [
            Attr.className "codicon codicon-gear"
          ]
        ]
      ]
      Html.div [
        Attr.className "sidebar-panel-content"
        toggleExpandedClass
        content |> DOM.fragment
      ]
    ]


  let plotlySidebarPanel obs =
    let chartFun (theme, charCount) =
      DemoChart.reactDemoChart theme [| 1.; 2.; 3.|] [| 1.; 4.; (float charCount) |]

    Html.div [
      Attr.className "sidebar-panel-chart"
      Bind.reactElement(obs, chartFun)
    ]


  let themeSelector (themeStore: VirtualStore<bool>) =
    let obsValues = [| true; false |]
    let labels = [|"Light"; "Dark"|]
    let indices = [| for i = 1 to labels.Length do string i|]

    let fromIndex i = Array.findIndex ((=) i) indices |> (fun i -> obsValues.[i])
    let toIndex b = Array.findIndex ((=) b) obsValues |> (fun i -> indices.[i])

    let mappedObs = themeStore |> VirtualStore.map(toIndex, fromIndex)

    Html.div [
      text "Theme:"
      Array.map2
        (fun i scoopChoice ->
          Html.label [
              class' "radio"
              Html.input [
                  type' "radio"
                  Bindings.bindRadioGroup mappedObs
                  i |> box |> Attr.value
              ]
              Html.span [
                text $" {scoopChoice}"
              ]
          ]
        )
        indices
        labels
      |> DOM.fragment
    ]

open Subelements

let sideBar (themeIsLightStore: VirtualStore<bool>) =
  let store, dispatch = () |> Store.makeElmish init update ignore
  let zippedStore = Store.zip (themeIsLightStore |> Store.distinct) store

  HtmlExt.recDivClass ["sidebar-container"; "sidebar-scrollable-content"] [
    store |> sideBarPanel dispatch "Global Settings" [
      themeSelector themeIsLightStore
    ]
    store |> sideBarPanel dispatch "Intro" [
      text "This side bar is resizable, try dragging the vertical separator."
      Html.br []
      text "The text editor is "
      Html.a [
        Attr.href "https://github.com/microsoft/monaco-editor"
        text "monaco-editor"
      ]
      text ", with all its features. Try "
      Html.kbd "Alt"; text "+"; Html.kbd "Shift"; text "+"; Html.kbd "F"
      text " for example."
    ]
    store |> sideBarPanel dispatch "Shortcuts" [
      Html.dl [
        Html.dt [ Html.kbd "Ctrl"; text "+"; Html.kbd "B" ]
        Html.dd [ text "Toggle the sidebar panels"]
        Html.dt [ Html.kbd "Ctrl"; text "+"; Html.kbd "S" ]
        Html.dd [ text "Calls the Save command"]
        Html.dt [ Html.kbd "Esc" ]
        Html.dd [ text "Release the keyboard focus (pressing Tab will then shift focus)"]
      ]
    ]
    store |> sideBarPanel dispatch "Demo Plotly chart" [
      text "A hosted react components that changes with the number of open panels."
      Html.br []
      text "Done with "
      Html.a [ text "Feliz.Plotly"; Attr.href "https://github.com/Shmew/Feliz.Plotly" ]
      Html.br []
      text "(Fable bindings for "
      Html.a [ text "plotly.js"; Attr.href "https://github.com/plotly/plotly.js" ]
      text " and "
      Html.a [ text "react-plotly.js"; Attr.href "https://github.com/plotly/react-plotly.js" ]
      text ")."
      zippedStore
        .> (fun (theme, m) -> theme, m.Expanded.Count)
        =/=|> plotlySidebarPanel
    ]
  ]
