module CellEditorPage

open Fable.Core.JsInterop
open Sutil
open Sutil.Attr
open Sutil.DOM
open SutilExt


let autofocusWhenTrue b : SutilElement =
  nodeFactory <| fun ctx ->
    if b then
      let e = ctx.ParentElement
      DOM.rafu (fun _ ->
        e.focus()
        e?setSelectionRange(99999,99999)
      )
    unitResult(ctx, "autofocusWhenTrue")

let toggleClassName(classesWhenTrue, classesWhenFalse) b: SutilElement =
  if b then
    nodeFactory ( fun ctx ->
      ctx.ParentElement |> removeFromClasslist classesWhenFalse
      ctx.ParentElement |> addToClasslist classesWhenTrue
      unitResult(ctx, "toggleClassName")
    )
  else
    nodeFactory ( fun ctx ->
      ctx.ParentElement |> removeFromClasslist classesWhenTrue
      ctx.ParentElement |> addToClasslist classesWhenFalse
      unitResult(ctx, "toggleClassName")
    )

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


let updateEditKeyDown cycle (ke: Browser.Types.KeyboardEvent) model =
  let length = model.Values.Length
  let unboundedActive =
    if ke.key = "Enter" && not ke.shiftKey then
      model.Active + 1
    elif ke.key = "Enter" && ke.shiftKey then
      model.Active - 1
    elif ke.key = "Tab" && not ke.shiftKey then
      model.Active + 1
    elif ke.key = "Tab" && ke.shiftKey then
      model.Active - 1
    else
      model.Active
  let outOfBound = unboundedActive < 0 || unboundedActive >= length
  if cycle then
    {model with Active= (unboundedActive + model.Values.Length) % model.Values.Length}
  else
    let newActive = if outOfBound then model.Active else unboundedActive
    {model with Active= newActive}


let updateNonEditKeyDown (ke: Browser.Types.KeyboardEvent) (model: Model) =
  let clamp n = if n < 0 then 0 elif n>= model.Values.Length then model.Values.Length - 1 else n
  if ke.key = "ArrowUp" then
    {model with Active = clamp (model.Active - 1)}
    ,Cmd.none
  elif ke.key = "ArrowDown" then
    {model with Active = clamp (model.Active + 1)}
    ,Cmd.none
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


let update msg model =
  match msg with
  | Msg.SelectCell i ->
      {model with Active = i; Editing = false}
      , Cmd.none
  | Msg.FinishEdit (value, ke) ->
      let newValues = Array.copy model.Values
      newValues.[model.Active] <- value

      {model with Values = newValues; Editing = false} |> updateEditKeyDown false ke,
      Cmd.none
  | Msg.StartEdit (i, v) ->
      {model with Active=i; Editing = true; EditingStartingValue= v}
      , Cmd.none
  | Msg.CancelEdit ->
      {model with Editing = false }, Cmd.none
  | Msg.NonEditKeyDown ke ->
      updateNonEditKeyDown ke model



let cellEditorPage dispatch (model: Store<Model>) =
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
    elif ke.ctrlKey  || ke.metaKey || ke.altKey then
      ()
    else
      ke.stopPropagation()

  let focusStore = Store.make ()
  HtmlExt.recDivClass [ "cell-editor-page"; ""; "table-container"] [
    attr("tabIndex", "0")
    bindSub focusStore (fun ctx () ->
      Browser.Dom.console.log "focusing parent"
      ctx.ParentElement.focus()
    )
    onKeyDown (fun ke -> Msg.NonEditKeyDown ke |> dispatch) []

    Html.div [
      Attr.className "table-container-title"
      text "An Input/Output Table"
    ]
    Html.div [ Attr.className "table-container-separator"]
    HtmlExt.recDivClass [ "table-container-content"; "table-content"] [
      Html.input [
        Attr.className "cell-editor-cell-active"
        type' "text"

        model .> (fun m -> m.Editing) |=/=> (fun editing -> [autofocusWhenTrue editing; Attr.hidden (not editing)] |> fragment)
        model .> (fun m -> $"grid-row-start: {m.Active+1}; grid-column-start: 2;") |=/=> Attr.style
        model .> (fun m -> m.EditingStartingValue |> Option.defaultValue m.Values.[m.Active]) |=/=> (fun v -> Attr.value v)

        onKeyDown (keyDownHandler focusStore dispatch) []
        on "blur" (fun _ -> Msg.CancelEdit |> dispatch) []
      ]

      model .> (fun m -> m.Labels.Length)
      |=/=>  (fun length ->
        [
          for i = 0 to length - 1 do
            Html.div [
              Attr.className "cell-editor-cell-inactive"
              model .> (fun m -> m.Labels.[i]) |=/=> (fun l -> text l)
            ]
            Html.div [
              Attr.className "cell-editor-cell-dormant"
              model .> (fun m -> m.Active = i && m.Editing) |=/=> Attr.hidden
              model .> (fun m -> m.Values.[i]) |=/=> text
              model .> (fun m -> m.Active = i) |=/=> toggleClassName("cell-editor-cell-dormant-selected", "")

              // Bind.toggleClass(model .> (fun m -> m.Active = i), "cell-editor-cell-dormant-selected", "")
              on "dblclick" (fun _ -> Msg.StartEdit (i, None) |> dispatch) [StopPropagation; PreventDefault]
              onClick (fun _ -> Msg.SelectCell i |> dispatch) []
            ]
        ] |> DOM.fragment)

    ]
  ]
