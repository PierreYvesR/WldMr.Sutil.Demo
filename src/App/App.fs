module App

open Sutil
open Sutil.Attr
open SutilExt
open SutilExt.Attr

type DragTarget =
  | NoTarget
  | PanelSplitter

[<RequireQualifiedAccess>]
type Page =
  | MonacoEditorPage
  | CellEditorPage

type Model = {
  DragTarget : DragTarget
  SidebarSize : float
  ThemeIsLight: bool

  TextStorage: TextStorage.Model
  MonacoEditorPage: MonacoEditorPage.Model
  CellEditorPage: CellEditorPage.Model

  // the bool is here so that we can keep the page the same
  // while still being 'distinct'
  // this way we can manage the focus consistently
  DisplayedPage: Page * bool
}
with
  static member isDragging(m: Model) = m.DragTarget = PanelSplitter
  static member sidebarSizeInt(m: Model) = m.SidebarSize |> int

type Msg =
  | PanelDragStarted
  | PanelDragEnded
  | MouseUp
  | MouseMove of Browser.Types.MouseEvent
  | DispatchResizeEvent
  | ThemeIsLight of bool

  | TextStorageMsg of TextStorage.Msg
  | MonacoEditorPageMsg of MonacoEditorPage.Msg
  | CellEditorPageMsg of CellEditorPage.Msg

  | LoadCheckpoint of int

  | ChangePage of Page


let private clamp min max value =
  if value >= max then max elif value <= min then min else value


let update msg (model : Model) =
  match msg with
  | MouseUp ->
      model,
      if model |> Model.isDragging then Cmd.ofMsg PanelDragEnded else Cmd.none

  | MouseMove position ->
      if model |> Model.isDragging then
        { model with SidebarSize = position.pageX - 5. |> clamp 250. 750. }
        , Cmd.ofMsg DispatchResizeEvent
      else
        model, Cmd.none

  | PanelDragStarted ->
      { model with DragTarget = PanelSplitter }
      , Cmd.none

  | PanelDragEnded ->
      { model with DragTarget = NoTarget }
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
      Cmd.ofMsg (MonacoEditorPageMsg (MonacoEditorPage.Msg.LoadText s))


  | MonacoEditorPageMsg subMsg ->
      let (subModel, subCmd, extMsg) =
        MonacoEditorPage.update subMsg model.MonacoEditorPage
      let extraCmd =
        match extMsg with
        | MonacoEditorPage.ExternalMsg.NoOp -> Cmd.none
        | MonacoEditorPage.ExternalMsg.SaveText s ->
            s |> TextStorage.Msg.SaveText |> TextStorageMsg |> Cmd.ofMsg
      { model with MonacoEditorPage = subModel},
      Cmd.batch [subCmd |> Cmd.map MonacoEditorPageMsg; extraCmd]

  | CellEditorPageMsg subMsg ->
      let (subModel, subCmd) =
        CellEditorPage.update subMsg model.CellEditorPage
      { model with CellEditorPage = subModel},
      subCmd |> Cmd.map CellEditorPageMsg

  | ChangePage page ->
      { model with DisplayedPage = (page, model.DisplayedPage |> snd |> not)},
      Cmd.none


let init () =
  {
    DragTarget= NoTarget
    SidebarSize= 400.
    ThemeIsLight= Interop.Window.matchMedia( "(prefers-color-scheme: light)" ).matches
    TextStorage= TextStorage.init ()
    MonacoEditorPage= MonacoEditorPage.initState ()
    CellEditorPage= CellEditorPage.initState ()
    DisplayedPage= Page.CellEditorPage, true
  },
  Cmd.batch [
    Hotkeys.Cmd.bindHotkey "alt+x" (Msg.ChangePage Page.CellEditorPage)

    Hotkeys.Cmd.bindHotkey "alt+t" (Msg.ChangePage Page.MonacoEditorPage)

    Cmd.ofMsgDelayed 0. DispatchResizeEvent
    MonacoEditorPage.initCmd() |> Cmd.map MonacoEditorPageMsg
    let initialText = """{"Here": "is some json","anObj": {"a": [1,2,3,5,8,13]}}"""
    MonacoEditorPage.Msg.LoadText initialText |> MonacoEditorPageMsg |> Cmd.ofMsg
    MonacoEditorPage.Msg.SaveEditor |> MonacoEditorPageMsg |> Cmd.ofMsg
    CellEditorPage.initCmd() |> Cmd.map CellEditorPageMsg
  ]

type Tree =
  {
    Key: string
    Value: string
    Children: Tree list
  }
  with
    static member children (t: Tree) = t.Children
    static member value (t: Tree) = t.Value
    static member key (t: Tree) = t.Key
    static member withElement f (t:Tree)= (t, Tree.children, Tree.value >> f, Tree.key)

let app () =
  let focusStore = Store.make ()
  let model, dispatch = () |> Store.makeElmish init update ignore

  let umedia = MediaQuery.listenMedia "(prefers-color-scheme: light)" (ThemeIsLight >> dispatch)
  let themeIsLight = model |> VirtualStore.ofStore  (fun m -> m.ThemeIsLight) (ThemeIsLight >> dispatch)

  let sidebarSize =
    model
    .> Model.sidebarSizeInt
    .>=/=> fun s -> $"grid-template-columns:{s}px 7px calc(100%% - {s+7}px)"

  let resizeHBar () =
    HtmlExt.recDivClass [ "horizontal-resize"; ""] [
      onMouse "mousedown" (fun _ -> dispatch PanelDragStarted) [PreventDefault]
      Html.div [ ]
    ]

  let tree =
    {
      Key= "Folder"; Value= "Folder"
      Children= [
        {Key= "File1"; Value= "File1"; Children=[]}
        {Key= "File2"; Value= "File2"; Children=[]}
        {
          Key= "SubFolder"; Value= "SubFolder"
          Children= [
            {Key= "File3"; Value= "File3"; Children=[]}
            {Key= "File4"; Value= "File4"; Children=[]}
          ]
        }
      ]

    }

  let panels =
    [
      Panels.MainMenuPanel.mainMenuPanel (ChangePage >> dispatch)
        [
          [
            text "Text Editor "
            Html.span [
              Attr.style "font-size: 12px;"
              Html.kbd "Alt"; text "+"; Html.kbd "T"
            ]
          ] |> DOM.fragment
          [
            text "Cell Editor "
            Html.span [
              Attr.style "font-size: 12px;"

              Html.kbd "Alt"; text "+"; Html.kbd "X"
            ]
          ] |> DOM.fragment
        ]
        [ Page.MonacoEditorPage; Page.CellEditorPage ]

      Panels.SettingsPanel.settingsPanel themeIsLight
      Panels.HelpPanels.introPanel
      Panels.HelpPanels.shortcutPanel
      Panels.CheckpointsPanel.checkpointsPanel (LoadCheckpoint >> dispatch) (model .> (fun m -> m.TextStorage.Checkpoints))
      Panels.PlotlyDemoPanel.plotlyDemoPanel
        (Store.zip themeIsLight (model .> (fun m ->
          m.CellEditorPage.CellValues
          |> Seq.skip (m.CellEditorPage.Dim.Col)
          |> Seq.indexed
          |> Seq.filter (fun (i, _) -> i % m.CellEditorPage.Dim.Col = 1)
          |> Seq.map (snd >> float)
          |> Seq.toArray)))
      Panels.TreeViewPanel.treeViewPanel ignore (tree |> Tree.withElement text)
    ]

  let mainPages =
    let makePage pageValue elt =
      Html.div [
        Attr.style "width:100%; height: 100%;"
        model .> (fun m -> (fst m.DisplayedPage) <> pageValue) |=/=> hiddenFixed
        // Bind.attr("hidden", model .> (fun m -> (fst m.DisplayedPage) <> pageValue) |> Store.distinct)
        elt
      ]

    [
      model .> (fun m -> m.DisplayedPage) |=/=> (Iter.eval (fun _ -> focusStore <~ ()))

      MonacoEditorPage.monacoEditorPage
        (MonacoEditorPageMsg >> dispatch)
        (model |> VirtualStore.ofStore (fun m -> m.MonacoEditorPage) ignore)
        focusStore
        themeIsLight
        |> makePage Page.MonacoEditorPage
      CellEditorPage.cellEditorPage
        (CellEditorPageMsg >> dispatch)
        (model |> VirtualStore.ofStore (fun m -> m.CellEditorPage) ignore)
        focusStore
        |> makePage Page.CellEditorPage
    ]

  HtmlExt.recDivClass [ "wm-app-container"; "wm-app-grid"] [
    DOM.unsubscribeOnUnmount [umedia]
    DOM.disposeOnUnmount [themeIsLight; model]

    let mouseMoveHandler (e: Browser.Types.Event) = e :?> Browser.Types.MouseEvent |> MouseMove |> dispatch
    let mouseUpHandler e = MouseUp |> dispatch

    model .> (fun m -> m |> Model.isDragging) |=/=> (fun isDragging ->
      [
        isDragging |> enableOnMouse("mouseup", mouseUpHandler)
        isDragging |> enableOnMouse("mousemove", mouseMoveHandler)
      ] |> DOM.fragment
    )

    bindElement sidebarSize Attr.style
    Sidebar.sideBar panels [Panels.MainMenuPanel.panelId; Panels.TreeViewPanel.panelId]
    resizeHBar()

    mainPages |> DOM.fragment
  ]
