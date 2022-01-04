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

let init () =
  {
    AllPanelsIds= Set.empty
    Expanded= Set.empty
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


let sideBar loadItemCallback (themeIsLightStore: VirtualStore<bool>) (textStorageStore: System.IObservable<TextStorage.Model>) =
  let store, dispatch = () |> Store.makeElmish init update ignore

  let panels =
    [
      Panels.SettingsPanel.settingsPanel themeIsLightStore
      Panels.HelpPanels.introPanel
      Panels.HelpPanels.shortcutPanel
      Panels.PlotlyDemoPanel.plotlyDemoPanel
        (Store.zip themeIsLightStore (store .> (fun m -> m.Expanded.Count)))
      Panels.CheckpointsPanel.checkpointsPanel loadItemCallback textStorageStore
    ]

  let allPanelsIds = panels |> List.map (fun p -> p.Id) |> Set.ofList

  store <~ { store.Value with AllPanelsIds= allPanelsIds }

  HtmlExt.recDivClass ["sidebar-container"; "sidebar-scrollable-content"] [
    for p in panels do
      yield store |> sideBarPanel dispatch p
  ]
