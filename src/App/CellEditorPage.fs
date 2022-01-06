module CellEditorPage

open Sutil
open Sutil.Attr
open SutilExt


open Sutil.DOM
let autofocus2 : SutilElement =
  nodeFactory <| fun ctx ->
      let e = ctx.ParentElement
      DOM.rafu (fun _ ->
          e.focus()
          )
      unitResult(ctx, "autofocus")


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
  | CancelEdit of int
  | DeferredCancelEdit of int
  | UpdateValue of (int * string * (Browser.Types.KeyboardEvent option))
  | NonEditKeyPress of Browser.Types.KeyboardEvent

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


let updateNonEditKey (ke: Browser.Types.KeyboardEvent) (model: Model) =
  let clamp n = if n < 0 then 0 elif n>= model.Values.Length then model.Values.Length - 1 else n
  // Browser.Dom.console.log ("NonEdit key", ke.key)
  if ke.key = "ArrowUp" then
    {model with Active = clamp (model.Active - 1)}
    ,Cmd.none
  elif ke.key = "ArrowDown" then
    {model with Active = clamp (model.Active + 1)}
    ,Cmd.none
  elif ke.key = "Enter" then
    {model with Active = (model.Active + 1) % model.Values.Length}
    ,Cmd.none
  elif ke.key = "F2" then
    model,
    Cmd.ofMsg (Msg.StartEdit (model.Active, None))
  elif ke.key.Length = 1 && (not (ke.ctrlKey || ke.altKey || ke.metaKey)) then
    let newValues = Array.copy model.Values
    {model with Values= newValues; Editing = true; EditingStartingValue= Some ke.key}, Cmd.none
  else
    model, Cmd.none


let updateActive (keOpt: Browser.Types.KeyboardEvent option) model =
  let inline updateActive_ (ke: Browser.Types.KeyboardEvent) =
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
    let newActive = if outOfBound then model.Active else unboundedActive
    {model with Active= newActive; Editing= false},
    if outOfBound then Cmd.none else Cmd.ofMsg (Msg.StartEdit (newActive, None))

  match keOpt with
  | None -> model, Cmd.none
  | Some ke -> updateActive_ ke

let update msg model =
  match msg with
  | Msg.SelectCell i ->
      {model with Active = i; Editing = false}
      , Cmd.none
  | Msg.UpdateValue (pos, value, keyEventOpt) ->
      // Browser.Dom.console.log ("UpdateValue")
      let newValues = Array.copy model.Values
      newValues.[pos] <- value

      {model with Values = newValues}
      |> updateActive keyEventOpt
  | Msg.StartEdit (i, v) ->
      // Browser.Dom.console.log ("start edit ✍", i)
      {model with Active=i; Editing = true; EditingStartingValue= v}
      , Cmd.none
  | Msg.DeferredCancelEdit i ->
      // Browser.Dom.console.log "DeferredCancelEdit"
      model, i |> Msg.CancelEdit |> Cmd.ofMsgDelayed 500.
  | Msg.CancelEdit i ->
      if model.Active = i then {model with Editing = false } else model
      , Cmd.none
  | Msg.NonEditKeyPress ke ->
      updateNonEditKey ke model


// open Sutil.DOM
open Fable.Core.JsInterop

let cellEditorPage dispatch (model: System.IObservable<Model>) =
  let focusStore = Store.make ()
  HtmlExt.recDivClass [ "cell-editor-page"; ""; "table-container"] [
    Html.div [
      Attr.className "table-container-title"
      text "An Input/Output Table"
    ]
    Html.div [ Attr.className "table-container-separator"]
    HtmlExt.recDivClass [ "table-container-content"; "table-content"] [
      DOM.attr("tabIndex","0")

      onKeyDown (fun ke ->
        Msg.NonEditKeyPress ke |> dispatch
      ) []


      Bindings.bindSub focusStore (fun ctx () ->
        // Browser.Dom.console.log "parent focus"
        ctx.ParentElement.focus()
      )


      model
      |=/=>  (fun m ->
        [
          // Browser.Dom.console.log("eval", m.Active, m.Editing)
          for i = 0 to m.Labels.Length - 1 do
            Html.div [
              Attr.className "cell-editor-cell-inactive"
              text m.Labels.[i]
            ]
            if m.Active = i && m.Editing then
              Html.input [
                Attr.className "cell-editor-cell-active"
                type' "text"
                Attr.value (m.EditingStartingValue |> Option.defaultValue m.Values.[i])
                autofocus2
                onKeyDown (fun ke ->

                  if ke.key = "Enter" || ke.key = "Tab" then
                    // Browser.Dom.console.log ("Enter/Tab", ke)
                    let loseFocus = updateActive (Some ke) m |> fst |> (fun m -> not m.Editing)
                    (i, ke.target?value, Some ke) |> Msg.UpdateValue |> dispatch
                    if loseFocus then DOM.rafu (fun () -> focusStore <~ ())
                    ke.preventDefault()
                    ke.stopPropagation()
                  elif ke.key = "Escape" then
                    // Browser.Dom.console.log ("Escape", ke)
                    Msg.CancelEdit i |> dispatch
                    DOM.rafu (fun () -> focusStore <~ ())
                    ke.stopPropagation()
                  elif ke.ctrlKey  || ke.metaKey || ke.altKey then
                    ()
                  else
                    ke.stopPropagation()
                ) []
                on "blur" (fun _ -> DOM.rafu (fun () -> Msg.DeferredCancelEdit i |> dispatch)) []
              ]
            else
              Html.div [
                Attr.className "cell-editor-cell-dormant"
                if m.Active = i then
                  Attr.className "cell-editor-cell-dormant-selected"
                text m.Values.[i]
                on "dblclick" (fun _ -> Msg.StartEdit (i, None) |> dispatch) []
                onClick (fun _ -> Msg.SelectCell i |> dispatch) []
              ]
        ] |> DOM.fragment)

    ]
  ]
