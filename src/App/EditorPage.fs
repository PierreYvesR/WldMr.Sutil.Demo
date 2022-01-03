module EditorPage

open Sutil
open Sutil.Attr
open SutilExt


[<RequireQualifiedAccess>]
type Msg =
  | SaveEditor
  | EditorCreated of Monaco.Monaco.Editor.IStandaloneCodeEditor
  | ContentChanged of Monaco.Monaco.Editor.IModelContentChangedEvent


type Model =
  {
    Editor: Monaco.Monaco.Editor.IStandaloneCodeEditor option
    CharCount: int
  }
  static member charCount(m: Model) = m.CharCount


let init () =
  {
    Editor= None
    CharCount= 0
  },
  Cmd.batch [
    Hotkeys.Cmd.bindHotkey "ctrl+s" Msg.SaveEditor
    Cmd.ofMsg Msg.SaveEditor
  ]


let update msg model =
  match msg with
  | Msg.EditorCreated e ->
      {model with Editor= Some e}, Cmd.none
  | Msg.SaveEditor ->
      let c = model.Editor |> Option.map (fun e -> e.getValue().Length) |> Option.defaultValue 0
      {model with CharCount= c}, Cmd.none
  | Msg.ContentChanged c ->
      let charCount = model.Editor |> Option.map (fun e -> e.getValue().Length) |> Option.defaultValue 0
      {model with CharCount= charCount}, Cmd.none


// themeIsLight should be a ReadOnlyStore<bool>
let editorPage (themeIsLight: VirtualStore<bool>) =
  let model, dispatch = () |> Store.makeElmish init update ignore

  HtmlExt.recDivClass [ "editor-page"; "editor-page-content"] [
    Html.div [
      Attr.className "editor-page-monaco-container"
      MonacoEditor.monacoEditor
        (Msg.EditorCreated >> dispatch)
        (Msg.ContentChanged >> dispatch)
        """{"Here": "is some json","anObj": {"a": [1,2,3,5,8,13]}}"""
        themeIsLight
    ]

    Html.div [ Attr.className "editor-page-separator"]

    Html.div [
      Attr.className "editor-page-control-bar"
      Html.button [
        Attr.className "wm-button"
        Html.text "Save"
        onClick (fun _ -> dispatch Msg.SaveEditor) []
      ]
      Html.span [
        Attr.style "margin-left: 10px"
        model .> Model.charCount |=/=> (sprintf "%d characters" >> text)
      ]

    ]
  ]
