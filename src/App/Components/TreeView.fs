module TreeView

open Sutil
open Sutil.Attr

open SutilExt
open SutilExt.Attr

type NodeId = string

type Model =
  {
    Expanded: Set<string>
  }
  static member isExpanded nodeId (m: Model) =
    m.Expanded.Contains nodeId
  static member toggleExpanded nodeId (m: Model) =
    if m |> Model.isExpanded nodeId then
      m.Expanded.Remove nodeId
    else
      m.Expanded.Add nodeId

[<RequireQualifiedAccess>]
type Msg =
  | ToggleNode of NodeId
  | HandleClick of NodeId

type TreeView<'T>(clickDispatch: NodeId -> unit) =
  member tv.init () = {
    Expanded= Set.empty
  }

  member tv.update msg model =
    match msg with
    | Msg.ToggleNode nodeId ->
        {model with Expanded= model |> Model.toggleExpanded nodeId}
    | Msg.HandleClick nodeId ->
        clickDispatch nodeId
        model



let treeView
  (clickDispatch)
  (
    tree: 'Tree,
    childrenF: 'Tree -> 'Tree list,
    contentF: 'Tree -> DOM.SutilElement,
    keyF: 'Tree -> string
  )
  =
  let rec treeViewElt dispatch (model: Store<Model>) (tree: 'Tree) =
    Html.div [
      let children = childrenF tree
      if children.IsEmpty then
        Html.div [
          Attr.className "treeview-item-container"
          Html.div [
            Attr.className "treeview-item-leaf-offset"
          ]
          Html.div [
            Attr.className "treeview-item-leaf"
            onClick ignore []
            contentF tree
          ]
        ]
      else
        let key = keyF tree
        Html.div [
          Html.div [
            Attr.classes ["treeview-item-container"; "treeview-item-parent"]
            Html.div [
              Bind.toggleClass(model .> Model.isExpanded key |> Store.distinct, "codicon codicon-chevron-down", "codicon codicon-chevron-right")
            ]
            onClick (fun _ -> key |> Msg.ToggleNode |> dispatch) []
            contentF tree
          ]
          Html.div [
            Attr.className "treeview-children"
            model .> (Model.isExpanded key >> not) |=/=> hiddenFixed
            children
            |> List.map (fun t ->
              treeViewElt dispatch model t
            ) |> DOM.fragment
          ]
        ]
    ]

  let tv = TreeView(clickDispatch)
  let model, dispatch = () |> Store.makeElmishSimple tv.init tv.update ignore
  Html.div [
    Attr.className "treeview-container"
    treeViewElt dispatch model tree
  ]
