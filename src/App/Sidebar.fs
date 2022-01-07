module Sidebar

open Sutil
open Sutil.Attr
open SutilExt


[<RequireQualifiedAccess>]
type Msg =
  | ToggleAll
  | Toggle of string

type Model =
  {
    AllPanelsIds: Set<string>
    Expanded: Set<string>
  }
  static member isExpanded panelId (m: Model) =
    m.Expanded.Contains panelId

let init (allPanelsIds, expandedIds) =
  {
    AllPanelsIds= allPanelsIds |> Set.ofList
    Expanded= expandedIds |> Set.ofList
  }, Cmd.batch [ Hotkeys.Cmd.bindHotkey "ctrl+b" Msg.ToggleAll ]

let update msg (model: Model) =
  match msg with
  | Msg.Toggle panelId ->
      DOM.rafu (fun _ -> DOM.dispatchSimple Browser.Dom.window "resize" )
      let newExpanded =
        if model |> Model.isExpanded panelId then
          model.Expanded.Remove panelId
        else
          model.Expanded.Add panelId
      { model with Expanded = newExpanded }
      , Cmd.none

  | Msg.ToggleAll ->
      DOM.rafu (fun _ -> DOM.dispatchSimple Browser.Dom.window "resize" )
      let newExpanded = if model.Expanded.IsEmpty then model.AllPanelsIds else Set.empty
      { model with Expanded = newExpanded }
      , Cmd.none


let private sideBarPanel dispatch (panel: Panels.Panel) (store: IStore<Model>) =
  let isExpanded =
    store
    |> VirtualStore.ofStore (Model.isExpanded panel.Id) ( fun _ -> Msg.Toggle panel.Id |> dispatch)

  Panels.sideBarPanel panel isExpanded


let sideBar (panels: Panels.Panel list) (expandedIds: string list) =
  let allPanelsIds = panels |> List.map (fun p -> p.Id)

  let store, dispatch = (expandedIds, allPanelsIds) |> Store.makeElmish init update ignore

  HtmlExt.recDivClass ["sidebar-container"; "sidebar-scrollable-content"] [
    for p in panels do
      yield store |> sideBarPanel dispatch p
  ]
