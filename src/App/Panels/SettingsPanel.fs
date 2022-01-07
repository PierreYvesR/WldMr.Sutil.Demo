module Panels.SettingsPanel

open Sutil
open Sutil.Attr
open SutilExt


let themeSelector (themeStore: VirtualStore<bool>) =
  let obsValues = [| true; false |]
  let labels = [|"Light"; "Dark"|]
  let indices = [| for i = 1 to labels.Length do string i|]

  let fromIndex i = Array.findIndex ((=) i) indices |> (fun i -> obsValues.[i])
  let toIndex b = Array.findIndex ((=) b) obsValues |> (fun i -> indices.[i])

  let mappedObs = themeStore |> VirtualStore.map(toIndex, fromIndex)

  Html.div [
    text "Theme:"
    Array.map2
      (fun i scoopChoice ->
        Html.div[
            class' "radio"
            Html.input [
                type' "radio"
                Bindings.bindRadioGroup mappedObs
                DOM.attr( "id", i)
                i |> box |> Attr.value
            ]
            Html.label [
              class' "radio"
              Attr.for' i
              text $" {scoopChoice}"
            ]
        ]
      )
      indices
      labels
    |> DOM.fragment
  ] :: []

let settingsPanel themeStore: Panel =
  {
    Content= themeSelector themeStore
    Title = "Settings"
    Id = "settings"
  }
