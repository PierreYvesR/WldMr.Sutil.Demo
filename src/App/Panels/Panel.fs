module Panels

open Sutil
open Sutil.Attr
open SutilExt


type Panel = {
  Id: string
  Title: string
  Content: seq<DOM.SutilElement>
}

let sideBarPanel (panel: Panel) (isExpandedStore: IVirtualStore<bool>) =
  let toggleExpandedClass = Bind.toggleClass(isExpandedStore, "wm-expanded", "wm-collapsed")
  Html.div [
    Attr.className "sidebar-panel"

    Html.div [
      Attr.className "sidebar-panel-header"
      toggleExpandedClass
      Html.div [
        Attr.style "display: flex; flex-grow: 1"
        Html.button [
          Attr.className "sidebar-panel-header-caption"
          onClick (fun _ -> isExpandedStore <~ not isExpandedStore.Value) []
          Html.div [
            Bind.toggleClass(isExpandedStore, "codicon codicon-chevron-down", "codicon codicon-chevron-right")
          ]
          Html.div [
            text panel.Title
          ]
        ]
      ]
      Html.button [
        Attr.className "sidebar-panel-header-options"
        onClick (fun _ -> Browser.Dom.console.log("gear")) [StopPropagation]
        Html.div [
          Attr.className "codicon codicon-gear"
        ]
      ]
    ]
    Html.div [
      Attr.className "sidebar-panel-content"
      toggleExpandedClass
      panel.Content |> DOM.fragment
    ]
  ]
