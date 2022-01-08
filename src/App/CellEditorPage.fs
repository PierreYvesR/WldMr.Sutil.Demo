module CellEditorPage

open Fable.Core.JsInterop
open Sutil
open Sutil.Attr
open Sutil.DOM
open SutilExt
open SutilExt.Attr


type Model =
  {
    Labels: string []
    Values: string []
    Active: int
    Editing: bool
    EditingStartingValue: string option
  }

[<RequireQualifiedAccess>]
type Msg =
  | SelectCell of int
  | StartEdit of int * string option
  | CancelEdit
  | FinishEdit of string * Browser.Types.KeyboardEvent
  | NonEditKeyDown of Browser.Types.KeyboardEvent
  | FocusOut of Browser.Types.FocusEvent
  | AddRow
  | DeleteSelectedRow

let initState () =
  {
    Labels= [| "EDH2"; "EDM2"; "EDU2"; "EDZ2" |]
    Values= [| "99.80"; "99.75"; "99.60"; "99.50"|]
    Active= 0
    Editing= false
    EditingStartingValue= None
  }

let initCmd () =
  Cmd.none

let private clamp length n = if n < 0 then 0 elif n>= length then length - 1 else n

module UpdateFns =

  let updateEditKeyDown cycle (ke: Browser.Types.KeyboardEvent) model =
    let length = model.Values.Length
    let unbounded =
      if (ke.key = "Enter" || ke.key = "Tab") then
        model.Active + (if ke.shiftKey then -1 else 1)
      else
        model.Active
    if cycle then
      {model with Active= (unbounded + length) % length}
    else
      {model with Active= clamp length unbounded}


  let updateNonEditKeyDown (ke: Browser.Types.KeyboardEvent) (model: Model) =
    let length = model.Values.Length
    if ke.eventPhase = 3 then
      model, Cmd.none
    elif ke.key = "PageUp" || (ke.key = "ArrowUp" && ke.ctrlKey) then
      {model with Active = 0}, Cmd.none
    elif ke.key = "PageDown" || (ke.key = "ArrowDown" && ke.ctrlKey) then
      {model with Active = length - 1}, Cmd.none
    elif ke.key = "ArrowUp" then
      {model with Active = clamp length (model.Active - 1)}, Cmd.none
    elif ke.key = "ArrowDown" then
      {model with Active = clamp length (model.Active + 1)}, Cmd.none
    elif ke.key = "Enter" then
      model |> updateEditKeyDown true ke
      ,Cmd.none
    elif ke.key = "F2" then
      model,
      Cmd.ofMsg (Msg.StartEdit (model.Active, None))
    elif ke.key.Length = 1 && (not (ke.ctrlKey || ke.altKey || ke.metaKey)) then
      model,
      Cmd.ofMsg (Msg.StartEdit (model.Active, Some ke.key))
    else
      model, Cmd.none

  let focusOutCmd (fe: Browser.Types.FocusEvent) =
    let target = fe.target :?> Browser.Types.Element
    if fe.relatedTarget = null then
      Msg.CancelEdit |> Cmd.ofMsg
    else
      let reTarget = fe.relatedTarget :?> Browser.Types.Element
      if reTarget <> null && (reTarget.contains(target) |> not) && (target.contains(reTarget) |> not) then
        Msg.CancelEdit |> Cmd.ofMsg
      else
        Cmd.none


let update msg model =
  Browser.Dom.console.log ("update msg=", msg)
  match msg with
  | Msg.SelectCell i ->
      {model with Active = i; Editing = false}
      , Cmd.none
  | Msg.FinishEdit (value, ke) ->
      let newValues = Array.copy model.Values
      newValues.[model.Active] <- value

      {model with Values = newValues; Editing = false} |> UpdateFns.updateEditKeyDown false ke,
      Cmd.none
  | Msg.StartEdit (i, v) ->
      {model with Active=i; Editing = true; EditingStartingValue= v}
      , Cmd.none
  | Msg.CancelEdit ->
      {model with Editing = false }, Cmd.none
  | Msg.NonEditKeyDown ke ->
      UpdateFns.updateNonEditKeyDown ke model
  | Msg.FocusOut fe ->
      model,
      UpdateFns.focusOutCmd fe

  | Msg.AddRow ->
      let newValues = Array.append model.Values [| "100." |]
      let newLabels = Array.append model.Labels [| "new" |]
      {model with Labels= newLabels; Values = newValues},
      Cmd.none

  | Msg.DeleteSelectedRow ->
      let newValues = Array.removeAt model.Active model.Values
      let newLabels = Array.removeAt model.Active model.Labels
      let newActive = clamp newLabels.Length model.Active
      {model with Labels= newLabels; Values = newValues; Active= newActive},
      Cmd.none

module SubParts =
  let editingCell dispatch model focusStore =
    // this function has focus side effects.
    let keyDownHandler focusStore dispatch (ke: Browser.Types.KeyboardEvent) =
      if ke.key = "Enter" || ke.key = "Tab" then
        (ke.target?value, ke) |> Msg.FinishEdit |> dispatch
        focusStore <~ ()
        ke.preventDefault()
        ke.stopPropagation()
      elif ke.key = "Escape" then
        Msg.CancelEdit |> dispatch
        focusStore <~ ()
        ke.stopPropagation()
      elif ke.ctrlKey || ke.metaKey || ke.altKey then
        ()
      else
        ke.stopPropagation()

    Html.input [
      Attr.className "cell-editor-cell-active"
      type' "text"

      model .> (fun m -> m.Editing) |=/=> (fun editing -> [autofocusWhenTrue editing; Attr.hidden (not editing)] |> fragment)
      model .> (fun m -> $"grid-row-start: {m.Active+1}; grid-column-start: 2;") |=/=> Attr.style
      model .> (fun m -> m.EditingStartingValue |> Option.defaultValue m.Values.[m.Active]) |=/=> (fun v -> Attr.value v)

      // that should accomplish the same thing, maybe a bit more efficient, certainly more error-prone
      //
      // SutilExt.bindVirtual (model |> Observable.pairwiseOpt) (fun (old, m) ->
      //   [
      //     if old |> Option.forall (fun o -> o.Editing <> m.Editing) then
      //       autofocusWhenTrue m.Editing
      //       Attr.hidden (not m.Editing)
      //       if m.Editing then
      //         Attr.style $"grid-row-start: {m.Active+1}; grid-column-start: 2;"
      //         Attr.value (m.EditingStartingValue |> Option.defaultValue m.Values.[m.Active])
      //   ] |> fragment
      // )

      onClick ignore [StopPropagation]
      onKeyDown (keyDownHandler focusStore dispatch) []
    ]


  let cellGrid dispatch modelStore length =
    [
      for i = 0 to length - 1 do
        Html.div [
          Attr.className "cell-editor-cell-inactive"
          modelStore .> (fun m -> m.Labels.[i]) |=/=> text
        ]
        Html.div [
          Attr.className "cell-editor-cell-dormant"
          modelStore .> (fun m -> m.Active = i && m.Editing) |=/=> Attr.hidden
          modelStore .> (fun m -> m.Values.[i]) |=/=> text
          modelStore .> (fun m -> m.Active = i) |=/=> toggleClassName("cell-editor-cell-dormant-selected", "")

          onDblclick (fun _ -> Msg.StartEdit (i, None) |> dispatch) [StopPropagation; PreventDefault]
          onClick (fun _ -> Msg.SelectCell i |> dispatch) []
        ]
    ] |> DOM.fragment

  let tableTtle () =
    Html.div [
      Attr.className "table-container-title"
      text "An Input/Output Table"
    ]

// modelStore should be a ReadOnlyStore
let cellEditorPage dispatch (modelStore: Store<_>) focusStore =
  HtmlExt.recDivClass ["cell-editor-page"; ""] [
    onMouse "mousedown" ignore [PreventDefault; StopPropagation] // we don't want to lose focus to an empty part of the page
    onClick (fun e ->
      // Browser.Dom.console.log("onClick page", e.eventPhase)
      Msg.CancelEdit |> dispatch
      focusStore <~ ()
    ) [PreventDefault; StopPropagation]
    HtmlExt.recDivClass ["table-container"] [
      attr("tabIndex", "0")
      focusStore |=> focus

      onKeyDown (fun e -> Browser.Dom.console.log("onKeyDown container", e.eventPhase); e |> Msg.NonEditKeyDown |> dispatch) []
      onFocusout (fun fe ->
        // Browser.Dom.console.log("focusout container", fe.eventPhase)
        fe |> Msg.FocusOut |> dispatch
      ) []
      onClick (fun e ->
        // Browser.Dom.console.log("onClick container", e.eventPhase)
        Msg.CancelEdit |> dispatch
      ) [StopPropagation]
      onMouse "mousedown" (fun e -> Browser.Dom.console.log("mousedown container", e.eventPhase)) [StopPropagation] // we don't let it bubble so that we don't lose focus

      SubParts.tableTtle ()
      Html.div [ Attr.className "table-container-separator"]
      HtmlExt.recDivClass [ "table-container-content"; "table-content"] [
        SubParts.editingCell dispatch modelStore focusStore

        modelStore .> (fun m -> m.Labels.Length) |=/=> SubParts.cellGrid dispatch modelStore
      ]
      Html.div [
        Attr.style "display: flex; justify-content: space-evenly; padding: 3px;"
        Html.button [
          Attr.className "wm-button"
          text "Add row"
          // onKeyDown ignore [StopPropagation]
          onClick (fun _ -> Msg.AddRow |> dispatch) []
        ]
        Html.button [
          Attr.className "wm-button"
          text "Delete selected row"
          // onKeyDown ignore [StopPropagation]
          onClick (fun _ -> Msg.DeleteSelectedRow |> dispatch) []
        ]
      ]
    ]
  ]
