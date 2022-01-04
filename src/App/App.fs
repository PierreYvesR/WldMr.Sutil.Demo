module App

open Sutil
open SutilExt
open Sutil.Attr

type DragTarget =
  | NoTarget
  | PanelSplitter

type Model = {
  DragTarget : DragTarget
  SidebarSize : float
  ThemeIsLight: bool
  TextStorage: TextStorage.Model
  EditorPage: EditorPage.Model
}
with
  static member isDragging(m: Model) = m.DragTarget = PanelSplitter
  static member sidebarSizeInt(m: Model) = m.SidebarSize |> int

type Msg =
  | PanelDragStarted
  | PanelDrag of Browser.Types.MouseEvent
  | PanelDragEnded
  | MouseUp
  | MouseMove of Browser.Types.MouseEvent
  | DispatchResizeEvent
  | ThemeIsLight of bool
  | TextStorageMsg of TextStorage.Msg
  | EditorPageMsg of EditorPage.Msg
  | LoadCheckpoint of int


let private clamp min max value =
  if value >= max then max elif value <= min then min else value


let update msg (model : Model) =
  match msg with
  | MouseUp ->
      model,
      if model |> Model.isDragging then Cmd.ofMsg PanelDragEnded else Cmd.none

  | MouseMove position ->
      model,
      if model |> Model.isDragging then Cmd.ofMsg (PanelDrag position) else Cmd.none

  | PanelDragStarted ->
      { model with DragTarget = PanelSplitter }
      , Cmd.none

  | PanelDragEnded ->
      { model with DragTarget = NoTarget }
      , Cmd.ofMsg DispatchResizeEvent

  | PanelDrag position ->
      { model with SidebarSize = position.pageX - 5. |> clamp 250. 750. }
      , Cmd.ofMsg DispatchResizeEvent

  | DispatchResizeEvent ->
      DOM.rafu (fun _ -> DOM.dispatchSimple Browser.Dom.window "resize" )
      model, Cmd.none

  | ThemeIsLight isLight ->
      // Browser.Dom.console.log("theme is light", isLight)
      DOM.rafu (fun _ ->
        let toRemove = if isLight then "dark-theme" else "light-theme"
        let toAdd = if isLight then "light-theme" else "dark-theme"
        Browser.Dom.document.body.classList.remove( toRemove)
        Browser.Dom.document.body.classList.add(toAdd)
      )

      { model with ThemeIsLight = isLight }
      , Cmd.none

  | TextStorageMsg subMsg ->
      let subModel =
        TextStorage.update model.TextStorage subMsg
      { model with TextStorage = subModel},
      Cmd.none

  | LoadCheckpoint idx ->
      let s = model.TextStorage.Checkpoints.Item(idx).Text
      model,
      Cmd.ofMsg (EditorPageMsg (EditorPage.Msg.LoadText s))


  | EditorPageMsg subMsg ->
      let (subModel, subCmd, extMsg) =
        EditorPage.update subMsg model.EditorPage
      let extraCmd =
        match extMsg with
        | EditorPage.ExternalMsg.NoOp -> Cmd.none
        | EditorPage.ExternalMsg.SaveText s -> s |> TextStorage.Msg.SaveText |> TextStorageMsg |> Cmd.ofMsg
      { model with EditorPage = subModel},
      Cmd.batch [subCmd |> Cmd.map EditorPageMsg; extraCmd]


let init () =
  {
    DragTarget= NoTarget
    SidebarSize= 400.
    ThemeIsLight= Interop.Window.matchMedia( "(prefers-color-scheme: light)" ).matches
    TextStorage= TextStorage.init ()
    EditorPage= EditorPage.initState ()
  },
  Cmd.batch [
    Cmd.move MouseMove
    Cmd.ups MouseUp
    Cmd.ofMsgDelayed 0. DispatchResizeEvent
    EditorPage.initCmd() |> Cmd.map EditorPageMsg
  ]

let app () =
  let model, dispatch = () |> Store.makeElmish init update ignore

  let umedia = MediaQuery.listenMedia "(prefers-color-scheme: light)" (dispatch << ThemeIsLight)

  let sidebarSize =
    model
    .> Model.sidebarSizeInt
    .>=/=> fun s -> $"grid-template-columns:{s}px 7px calc(100%% - {s+7}px)"

  let themeIsLight = model |> VirtualStore.ofStore  (fun m -> m.ThemeIsLight) (ThemeIsLight >> dispatch)

  let resizeHBar () =
    HtmlExt.recDivClass [ "horizontal-resize"; ""] [
      onMouse "mousedown" (fun _ -> dispatch PanelDragStarted) [PreventDefault]
      Html.div [ ]
    ]


  HtmlExt.recDivClass [ "wm-app-container"; "wm-app-grid"] [
    DOM.unsubscribeOnUnmount [ umedia; themeIsLight.Dispose]
    bindElement sidebarSize Attr.style

    Sidebar.sideBar (LoadCheckpoint >> dispatch) themeIsLight (model .> (fun m -> m.TextStorage))
    resizeHBar()
    EditorPage.editorPage (EditorPageMsg >> dispatch) (model .> fun m -> m.EditorPage) themeIsLight
  ]
