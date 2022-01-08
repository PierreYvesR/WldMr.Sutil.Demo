module Panels.HelpPanels

open Sutil
open Sutil.Attr


let shortcutPanel: Panels.Panel = {
  Title= "Shortcuts"; Id= "shortcut";
  Content = [
    text "Shortcuts provided by "
    Html.a [
      Attr.href "https://github.com/jaywcjlove/hotkeys"
      Attr.target "_blank"
      text "HotKeys.js"
    ]
    Html.dl [
      Html.dt [ Html.kbd "Ctrl"; text "+"; Html.kbd "B" ]
      Html.dd [ text "Toggle the sidebar panels"]
      Html.dt [ Html.kbd "Ctrl"; text "+"; Html.kbd "S" ]
      Html.dd [ text "Calls the Save command"]
      Html.dt [ Html.kbd "Esc" ]
      Html.dd [ text "Release the keyboard focus (pressing Tab will then shift focus)"]
    ]
]}

let introPanel: Panels.Panel = {
  Title= "Intro"; Id= "intro";
  Content= [
    text "This side bar is resizable, try dragging the vertical separator."
    Html.br []
    text "The text editor is "
    Html.a [
      Attr.href "https://github.com/microsoft/monaco-editor"
      Attr.target "_blank"
      text "monaco-editor"
    ]
    text ", with all its features. Try "
    Html.kbd "Alt"; text "+"; Html.kbd "Shift"; text "+"; Html.kbd "F"
    text " for example."
]}
