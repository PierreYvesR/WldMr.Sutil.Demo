module Panels.CheckpointsPanel
open System

open Sutil
open Sutil.Attr
open Sutil.Styling

open Fable.Core.JsInterop

let checkpointsPanel (loadItemCallback) (textStore: IObservable<TextStorage.Model>): Panel =
  let choice (i, cp: TextStorage.Checkpoint) =
    let date = cp.SaveTime.ToString("yyyy-MM-dd hh:mm:ss")
    Html.option [
      DOM.attr("value", $"{i}") 
      text $"{date}, {cp.LineCount} lines"
    ]
  let choices () =
    let indexedCheckpoint = (textStore .> (fun tsm -> tsm.Checkpoints))
    Bind.eachi(indexedCheckpoint, choice) 

  let select tsm =
    Html.div [
      text "select"
      Html.select [
        on "change" ( fun e -> 
          let value: string = e?target?value
          loadItemCallback (value |> int)
        ) []
        choices ()
      ]
    ]

  let content () =
    Html.div [
      textStore |=> (fun ts -> text $"{ts.Checkpoints.Length}")
      select textStore
    ]  


  {
    Content= [content()]
    Title = "Checkpoints"
    Id = "checkpoints"
  }
