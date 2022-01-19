module CellEditorPage

open Fable.Core.JsInterop
open Sutil
open Sutil.Attr
open Sutil.DOM
open SutilExt
open SutilExt.Attr

let private clamp length n = if n < 0 then 0 elif n>= length then length - 1 else n

[<ReferenceEquality>]
type EditablePredicate =
  {
    p: int -> int -> bool
  }

type CellPos =
  {Row: int; Col: int}
  with
    static member plusRow i (cp: CellPos) = {cp with Row= cp.Row + i}
    static member plusCol i (cp: CellPos) = {cp with Col= cp.Col + i}
    static member setRow i (cp: CellPos) = {cp with Row= i}
    static member setCol i (cp: CellPos) = {cp with Col= i}
    static member clamp (dim: CellPos) (cp: CellPos) =
      {
        Row= clamp dim.Row cp.Row
        Col= clamp dim.Col cp.Col
      }
    static member wrapRow (dim: CellPos) (cp: CellPos) =
      let newCol = clamp dim.Col cp.Col
      let colOvershoot = cp.Col - newCol
      let rowChange = colOvershoot |> min 1 |> max -1
      let newRow = cp.Row + rowChange |> clamp dim.Row
      {
        Row= newRow
        Col= if newRow > cp.Row then 0 elif newRow < cp.Row then dim.Col - 1 else newCol
      }


type Model =
  {
    Dim: CellPos
    CellValues: string []
    Active: CellPos
    Editing: bool
    EditingStartingValue: string option
    EditablePredicate: EditablePredicate
  }
  with
    member m.getValue(cp: CellPos) =
      m.CellValues.[cp.Row * m.Dim.Col + cp.Col]

    member m.isEditable(cp: CellPos) =
      m.EditablePredicate.p cp.Row cp.Col


[<RequireQualifiedAccess>]
type Msg =
  | SelectCell of CellPos
  | StartEdit of CellPos * string option
  | CancelEdit
  | FinishEdit of string * Browser.Types.KeyboardEvent
  | NonEditKeyDown of Browser.Types.KeyboardEvent
  | AddRow
  | DeleteSelectedRow

let initState () =
  {
    Dim= {Row= 5; Col= 2}
    CellValues= [| "Name"; "Price"; "EDH2"; "99.80"; "EDM2"; "99.75"; "EDU2"; "99.60"; "EDZ2"; "99.50"|]
    Active= {Row= 0; Col= 0}
    Editing= false
    EditingStartingValue= None
    EditablePredicate= {p= fun i j -> i > 0}
  }

let initCmd () =
  Cmd.none


module UpdateFns =

  let updateEditKeyDown cycle (ke: Browser.Types.KeyboardEvent) model =
    let dim = model.Dim
    let unbounded =
      if ke.key = "Enter" then
        model.Active |> CellPos.plusRow (if ke.shiftKey then -1 else 1)
      elif ke.key = "Tab" then
        model.Active
        |> CellPos.plusCol (if ke.shiftKey then -1 else 1)
        |> CellPos.wrapRow dim
      else
        model.Active
    {model with Active= unbounded |> CellPos.clamp dim}


  let updateNonEditKeyDown (ke: Browser.Types.KeyboardEvent) (model: Model) =
    let dim = model.Dim
    if ke.eventPhase = 3 then
      model, Cmd.none
    elif ke.key = "PageUp" || (ke.key = "ArrowUp" && ke.ctrlKey) then
      {model with Active = model.Active |> CellPos.setRow 0}, Cmd.none
    elif ke.key = "PageDown" || (ke.key = "ArrowDown" && ke.ctrlKey) then
      {model with Active = model.Active |> CellPos.setRow (dim.Row - 1)}, Cmd.none
    elif ke.key = "ArrowUp" then
      {model with Active = model.Active |> CellPos.plusRow -1 |> CellPos.clamp dim}, Cmd.none
    elif ke.key = "ArrowDown" then
      {model with Active = model.Active |> CellPos.plusRow 1 |> CellPos.clamp dim}, Cmd.none
    elif ke.key = "ArrowLeft" then
      {model with Active = model.Active |> CellPos.plusCol -1 |> CellPos.clamp dim}, Cmd.none
    elif ke.key = "ArrowRight" then
      {model with Active = model.Active |> CellPos.plusCol 1 |> CellPos.clamp dim}, Cmd.none
    elif ke.key = "Enter" then
      model |> updateEditKeyDown true ke
      ,Cmd.none
    elif model.isEditable model.Active then
      if ke.key = "F2" then
        model,
        Cmd.ofMsg (Msg.StartEdit (model.Active, None))
      elif ke.key.Length = 1 && (not (ke.ctrlKey || ke.altKey || ke.metaKey)) then
        model,
        Cmd.ofMsg (Msg.StartEdit (model.Active, Some ke.key))
      else
        model, Cmd.none
    else
      model, Cmd.none

  let focusOutHandler dispatch (fe: Browser.Types.FocusEvent) =
    let target = fe.target :?> Browser.Types.Element
    if fe.relatedTarget = null then
      Msg.CancelEdit |> dispatch
    else
      let reTarget = fe.relatedTarget :?> Browser.Types.Element
      if reTarget <> null && (reTarget.contains(target) |> not) && (target.contains(reTarget) |> not) then
        Msg.CancelEdit |> dispatch


let update msg model =
  match msg with
  | Msg.SelectCell i ->
      {model with Active = i; Editing = false}
      , Cmd.none
  | Msg.FinishEdit (value, ke) ->
      let newValues = Array.copy model.CellValues

      newValues.[model.Active.Row * model.Dim.Col + model.Active.Col] <- value

      {model with CellValues = newValues; Editing = false} |> UpdateFns.updateEditKeyDown false ke,
      Cmd.none
  | Msg.StartEdit (i, v) ->
      {model with Active=i; Editing = true; EditingStartingValue= v}
      , Cmd.none
  | Msg.CancelEdit ->
      {model with Editing = false }, Cmd.none
  | Msg.NonEditKeyDown ke ->
      UpdateFns.updateNonEditKeyDown ke model

  | Msg.AddRow ->
      let newValues = Array.append model.CellValues [| ""; "0." |]
      {model with CellValues = newValues; Dim= model.Dim |> CellPos.plusRow 1},
      Cmd.none

  | Msg.DeleteSelectedRow ->
      let canDelete =
        [0..model.Dim.Col-1]
        |> Seq.forall (model.EditablePredicate.p model.Active.Row)
      if canDelete then
        let newDim = model.Dim |> CellPos.plusRow -1
        let newValues = model.CellValues |> Array.removeManyAt (model.Active.Row * model.Dim.Col) model.Dim.Col
        let newActive = model.Active |> CellPos.clamp newDim
        {model with Dim= newDim; CellValues = newValues; Active= newActive},
        Cmd.none
      else
        model, Cmd.none

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

    Html.div [
      Attr.className "cell-input-container"

      model .> (fun m -> not m.Editing) |=/=> hiddenFixed
      model .> (fun m -> $"grid-row-start: {m.Active.Row+1}; grid-column-start: {m.Active.Col+1};") |=/=> Attr.style
      Html.input [
        Attr.className "cell-input"
        type' "text"

        // model .> (fun m -> m.Editing) |=/=> (fun editing -> [autofocusWhenTrue editing; hiddenFixed (not editing)] |> fragment)
        // model .> (fun m -> $"grid-row-start: {m.Active.Row+1}; grid-column-start: {m.Active.Col+1};") |=/=> Attr.style
        // model .> (fun m -> m.EditingStartingValue |> Option.defaultValue (m.getValue(m.Active))) |=/=> (fun v -> Attr.value v)

        // that should accomplish the same thing, maybe a bit more efficient, certainly more error-prone, (later-on) actually...
        //
        SutilExt.bindVirtual (model |> Observable.pairwiseOpt) (fun (old, m) ->
          [
            if old |> Option.forall (fun o -> o.Editing <> m.Editing) then
              autofocusWhenTrue m.Editing
              // hiddenFixed (not m.Editing)
              if m.Editing then
                // Attr.style $"grid-row-start: {m.Active.Row+1}; grid-column-start: {m.Active.Col+1};"
                Attr.value (m.EditingStartingValue |> Option.defaultValue (m.getValue m.Active))
          ] |> fragment
        )

        onClick ignore [StopPropagation]
        onKeyDown (keyDownHandler focusStore dispatch) []
      ]
    ]

  let cellGrid dispatch (modelStore: Store<Model>) (dim: CellPos) =
    [
      for i = 0 to dim.Row - 1 do
        for j = 0 to dim.Col - 1 do
          let pos = {Row= i; Col= j}
          Html.div [
            Attr.className "cell-editor-cell-dormant"
            if modelStore.Value.isEditable pos |> not then
              Attr.className "cell-readonly"
            modelStore .> (fun m -> m.Active = pos && m.Editing) |=/=> hiddenFixed
            HtmlExt.recDivClass ["cell-content"] [
              modelStore .> (fun m -> m.getValue(pos)) |=/=> text
            ]
            modelStore .> (fun m -> m.Active = pos) |=/=> toggleClassName("cell-selected", "")

            onClick (fun _ -> Msg.SelectCell pos |> dispatch) []
            if modelStore.Value.isEditable pos then
              onDblclick (fun _ -> Msg.StartEdit (pos, None) |> dispatch) [StopPropagation; PreventDefault]
          ]
    ] |> DOM.fragment

  let tableTtle () =
    Html.div [
      Attr.className "table-container-title"
      text "An Input/Output Table"
    ]

  let bottomButtons dispatch =
    Html.div [
      Attr.className "table-container-bottom"
      Html.button [
        Attr.className "small-inline-button"
        text "Add row"
        onClick (fun _ -> Msg.AddRow |> dispatch) []
      ]
      Html.button [
        Attr.className "small-inline-button"
        text "Delete selected row"
        onClick (fun _ -> Msg.DeleteSelectedRow |> dispatch) []
      ]
    ]


// modelStore should be a ReadOnlyStore
let cellEditorPage dispatch (modelStore: Store<_>) focusStore =
  HtmlExt.recDivClass ["cell-editor-page"; ""] [
    onMouse "mousedown" ignore [PreventDefault; StopPropagation] // we don't want to lose focus to an empty part of the page
    onClick (fun e ->
      if modelStore.Value.Editing then Msg.CancelEdit |> dispatch
      focusStore <~ ()
    ) [PreventDefault; StopPropagation]

    HtmlExt.recDivClass ["table-container"] [
      attr("tabIndex", "0")
      focusStore |=> focus

      onKeyDown (Msg.NonEditKeyDown >> dispatch) []
      onFocusout (UpdateFns.focusOutHandler dispatch) []
      onClick (fun _ -> if modelStore.Value.Editing then Msg.CancelEdit |> dispatch ) [StopPropagation]
      onMouse "mousedown" ignore [StopPropagation] // we don't let it bubble so that we don't lose focus

      SubParts.tableTtle ()
      Html.div [ Attr.className "table-container-separator"]
      HtmlExt.recDivClass [ "table-container-content"; "table-content"] [
        SubParts.editingCell dispatch modelStore focusStore
        modelStore .> (fun m -> m.Dim) |=/=> SubParts.cellGrid dispatch modelStore
      ]
      SubParts.bottomButtons dispatch
    ]
  ]
