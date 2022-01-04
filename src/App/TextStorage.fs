module TextStorage

open Sutil

type Checkpoint = {
  Text: string
  LineCount: int
  SaveTime: System.DateTime
}

type Model = {
  Checkpoints: Checkpoint list
}

type Msg =
  | SaveText of string

open Fable.Core

[<Emit("new Date(Date.now())")>]
let private now() : System.DateTime = jsNative

let update model msg =
  match msg with
  | SaveText s ->
      let lineCount = s.Split("\n").Length
      let saveTime = System.DateTime.Now
      {
          model with Checkpoints= model.Checkpoints @ [{Text=s; LineCount=lineCount; SaveTime=saveTime}]
      }

let init (): Model = // * Cmd<Msg>
  {Checkpoints= []}