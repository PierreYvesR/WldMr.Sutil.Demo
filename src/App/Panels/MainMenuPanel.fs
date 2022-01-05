module Panels.MainMenuPanel

open Sutil
open Sutil.Attr

let panelId = "mainmenu"

let mainMenuPanel dispatch titles msgs: Panel =
  let content () =
    (titles, msgs)
    ||> List.map2 (fun title msg ->
      Html.button [
        Attr.className "wm-button"
        Html.text (title: string)
        onClick (fun _ -> dispatch msg) []
      ]
    )
  {
    Content= content()
    Title = "Main menu"
    Id = panelId
  }
