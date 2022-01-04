module EditorPage

open Sutil
open Sutil.Attr
open SutilExt


[<RequireQualifiedAccess>]
type Msg =
  | SaveEditor
  | EditorCreated of Monaco.Monaco.Editor.IStandaloneCodeEditor
  | ContentChanged of Monaco.Monaco.Editor.IModelContentChangedEvent
  | LoadText of string


type Model =
  {
    Editor: Monaco.Monaco.Editor.IStandaloneCodeEditor option
    CharCount: int
  }
  static member charCount(m: Model) = m.CharCount

type ExternalMsg =
  | NoOp
  | SaveText of string

let initState () =
  {
    Editor= None
    CharCount= 0
  }
let initCmd () =
  Cmd.batch [
    Hotkeys.Cmd.bindHotkey "ctrl+s" Msg.SaveEditor
    Cmd.ofMsg Msg.SaveEditor
  ]


let update msg model =
  match msg with
  | Msg.EditorCreated e ->
      let charCount = e.getValue().Length
      {model with Editor= Some e; CharCount= charCount}, Cmd.none, ExternalMsg.NoOp
  | Msg.SaveEditor ->
      model, Cmd.none, (
        model.Editor |> Option.map (MonacoEditor.getValue >> ExternalMsg.SaveText)
        |> Option.defaultValue ExternalMsg.NoOp
      )
  | Msg.ContentChanged c ->
      let charCount =
        model.Editor |> Option.map (MonacoEditor.getValue >> (fun s -> s.Length)) |> Option.defaultValue 0
      {model with CharCount= charCount}, Cmd.none, ExternalMsg.NoOp
  | Msg.LoadText c ->
      model.Editor |> Option.iter ( fun e ->
        e.setValue c
      )
      {model with CharCount = c.Length}
      , Cmd.none, ExternalMsg.NoOp


// themeIsLight should be a ReadOnlyStore<bool>
let editorPage dispatch (model: System.IObservable<Model>) (themeIsLight: VirtualStore<bool>) =
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
