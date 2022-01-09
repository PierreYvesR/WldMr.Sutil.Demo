module Panels.CheckpointsPanel
open System

open Sutil
open Sutil.Attr

open Fable.Core.JsInterop

let checkpointsPanel (loadItemCallback) (checkpointsObs: IObservable<TextStorage.Checkpoint list>): Panel =
  let choice (i, cp: TextStorage.Checkpoint) =
    let date = cp.SaveTime.ToString("yyyy-MM-dd hh:mm:ss")
    Html.option [
      DOM.attr("value", $"{i}")
      text $"{date}, {cp.LineCount} lines"
    ]
  let choices () =
    Bind.eachi(checkpointsObs, choice)

  let select () =
    Html.div [
      text "Choose a version to revert to"
      Html.br []
      Html.select [
        on "change" ( fun e ->
          let value: string = e?target?value
          loadItemCallback (value |> int)
          e?target?value <- "-1"
        ) []
        Html.option [
          Attr.selected true
          Attr.disabled true
          Attr.hidden true
          Attr.value  "-1"
          text "select a version to load"
        ]
        choices ()
      ]
    ]

  let content () =
    Html.div [
      checkpointsObs |=> (fun cps -> text $"{cps.Length} checkpoint(s) saved.")
      select ()
    ]


  {
    Content= [content()]
    Title = "Checkpoints"
    Id = "checkpoints"
  }
