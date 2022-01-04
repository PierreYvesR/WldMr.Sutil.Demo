module Panels.PlotlyDemoPanel

open Sutil
open Sutil.Attr
open SutilExt


let private plotlyPanelChart obs =
  let chartFun (theme, charCount) =
    DemoChart.reactDemoChart theme [| 1.; 2.; 3.|] [| 1.; 4.; (float charCount) |]

  Html.div [
    Attr.className "sidebar-panel-chart"
    Bind.reactElement(obs, chartFun)
  ]


let plotlyDemoPanel zippedStore: Panels.Panel  =
  {
    Title= "Demo Plotly chart"; Id= "plotly-demo";
    Content= [
      text "A hosted react components that changes with the number of open panels."
      Html.br []
      text "Done with "
      Html.a [ text "Feliz.Plotly"; Attr.href "https://github.com/Shmew/Feliz.Plotly" ]
      Html.br []
      text "(Fable bindings for "
      Html.a [ text "plotly.js"; Attr.href "https://github.com/plotly/plotly.js" ]
      text " and "
      Html.a [ text "react-plotly.js"; Attr.href "https://github.com/plotly/react-plotly.js" ]
      text ")."
      zippedStore
        =/=|> plotlyPanelChart
  ]}
