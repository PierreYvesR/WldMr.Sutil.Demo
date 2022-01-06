module Panels.MainMenuPanel

open Sutil
open Sutil.Attr

let panelId = "mainmenu"

let mainMenuPanel dispatch contents msgs: Panel =
  let content () =
    Html.div [
      Attr.className "mainmenu-panel"
      (contents, msgs)
      ||> List.map2 (fun content msg ->
        Html.button [
          Attr.className "wm-button"
          content
          onClick (fun _ -> dispatch msg) []
        ]
      )
      |> DOM.fragment
    ]

  {
    Content= [content()]
    Title = "Main menu"
    Id = panelId
  }
