module Panels.TreeViewPanel


let panelId = "treeview-test"

let treeViewPanel clickDispatch treeData: Panel =
  {
    Content= [
      TreeView.treeView clickDispatch treeData
     ]
    Title = "TreeView test"
    Id = panelId
  }
