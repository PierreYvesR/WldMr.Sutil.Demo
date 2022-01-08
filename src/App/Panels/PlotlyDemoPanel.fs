module Panels.PlotlyDemoPanel

open Sutil
open Sutil.Attr
open SutilExt


let private plotlyPanelChart obs =
  let chartFun (theme, v: float[]) =
    let xs = Array.init v.Length (fun i -> i + 1 |> float)
    DemoChart.reactDemoChart theme xs v

  Html.div [
    Attr.className "sidebar-panel-chart"
    Bind.reactElement(obs, chartFun)
  ]

let panelId = "plotly-demo"

let plotlyDemoPanel zippedStore: Panels.Panel =
  {
    Title= "Demo Plotly chart"; Id= panelId;
    Content= [
      text "A hosted react component that reflect the cells in the editor."
      Html.br []
      text "Done with "
      Html.a [ text "Feliz.Plotly"; Attr.href "https://github.com/Shmew/Feliz.Plotly" ]
      Html.br []
      text "(Fable bindings for "
      Html.a [ text "plotly.js"; Attr.href "https://github.com/plotly/plotly.js" ]
      text " and "
      Html.a [ text "react-plotly.js"; Attr.href "https://github.com/plotly/react-plotly.js" ]
      text ")."
      Html.div [ Attr.style "height: 2px;"]
      zippedStore
        =/=|> plotlyPanelChart
  ]}
