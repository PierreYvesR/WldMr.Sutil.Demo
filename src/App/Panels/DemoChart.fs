module DemoChart

open Feliz
open Feliz.Plotly


let reactDemoChart theme (xs: float[]) (ys: float[]) =
  Plotly.plot [
    plot.traces [
      traces.scatter [
        scatter.x xs
        scatter.y ys
        scatter.mode [
          scatter.mode.lines
          scatter.mode.markers
        ]
      ]
    ]
    plot.layout [
      // layout.title "a Chart"
      layout.autosize true
      layout.paperBgcolor
        (
          if theme then
            ThemeColors.Light.``wm-bg1-color``
          else
            ThemeColors.Dark.``wm-bg1-color``
        )
      layout.plotBgcolor
        (
          if theme then
            ThemeColors.Light.``wm-bg4-color``
          else
            ThemeColors.Dark.``wm-bg4-color``
        )
      layout.xaxis [
        xaxis.color
          (
            if theme then
              ThemeColors.Light.``wm-bg9-color``
            else
              ThemeColors.Dark.``wm-bg9-color``
          )
      ]
      layout.yaxis [
        yaxis.color
          (
            if theme then
              ThemeColors.Light.``wm-bg9-color``
            else
              ThemeColors.Dark.``wm-bg9-color``
          )
      ]
      layout.margin [
        margin.t 40
        margin.b 30
        margin.r 20
        margin.l 20
      ]
    ]
    plot.useResizeHandler true
    plot.style [
      Interop.mkStyle "height" "100%"
      Interop.mkStyle "width" "100%"
    ]

    plot.config [
      config.displaylogo false
      config.showAxisDragHandles true
      config.responsive true
    ]
  ]
