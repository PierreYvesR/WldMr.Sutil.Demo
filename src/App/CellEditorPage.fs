module CellEditorPage

open Sutil
open Sutil.Attr
open SutilExt


let cellEditorPage () =
  HtmlExt.recDivClass [ "cell-editor-page"; ""] [
    Html.div [
      Attr.className "cell-editor-content"
      text "<Cell Editor placeholder>"
    ]
  ]
