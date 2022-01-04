module MonacoEditor

open Fable.Core
open Sutil
open SutilExt
open Monaco


[<AutoOpen>]
module private Internal =
  let monacoSetup () =
    Monaco.editor.defineTheme(
      "wm-theme-light",
        {
          ``base`` = Monaco.Editor.BuiltinTheme.Vs
          ``inherit`` = true
          colors = JsInterop.jsOptions<Monaco.Editor.IColors>(
            fun o ->
              o.["editor.background"] <- ThemeColors.Light.``wm-bg3-color-hex``
              o.["editor.lineHighlightBackground"] <- "#007AFF14"
            )
          rules =
            let rule: Monaco.Editor.ITokenThemeRule = {
              Monaco.Editor.ITokenThemeRule.token= ""
              foreground= None
              background= Some ThemeColors.Light.``wm-bg1-color-hex``
              fontStyle= None
            }
            [rule] |> ResizeArray
          encodedTokensColors = None
        }
    )
    Monaco.editor.defineTheme(
      "wm-theme-dark",
        {
          ``base`` = Monaco.Editor.BuiltinTheme.VsDark
          ``inherit`` = true
          colors = JsInterop.jsOptions<Monaco.Editor.IColors>(
              fun o ->
                o.["editor.background"] <- ThemeColors.Dark.``wm-bg2-color-hex``
                o.["editor.lineHighlightBackground"] <- "#007AFF14"
            )
          rules =
            let rule: Monaco.Editor.ITokenThemeRule = {
              token= ""
              foreground= None
              background= Some ThemeColors.Dark.``wm-bg1-color-hex``
              fontStyle= None
            }
            [rule] |> ResizeArray
          encodedTokensColors = None
        }
      )

  do monacoSetup()

  let monacoEditorOptions initialText themeName =
    JsInterop.jsOptions<Monaco.Editor.IStandaloneEditorConstructionOptions>(
      fun o ->
        o.language <- "json" |> Some
        o.theme <- themeName |> Some
        o.value <- initialText |> Some
        o.scrollBeyondLastLine <- false |> Some
        o.minimap <-
          (
            JsInterop.jsOptions<Monaco.Editor.IEditorMinimapOptions>(
              fun o ->
                o.size <- Monaco.Editor.IEditorMinimapOptionsSize.Proportional |> Some
                o.scale <- 40. |> Some
            ) |> Some
          )
    )

  let themeName isLight =
    if isLight then "wm-theme-light" else "wm-theme-dark"

  type MonacoDisposable(monacoDisposable: Monaco.IDisposable) =
    interface System.IDisposable with
      member _.Dispose() = monacoDisposable.dispose()



let setTheme isLight=
  Monaco.editor.setTheme( themeName isLight )

let getValue (e: Monaco.Editor.ICodeEditor) =
  e.getValue()

// this should be a ReadOnlyStore
let monacoEditor onCreated onChange initialText (isLightTheme: Store<bool>) =
  Html.div [
    Attr.style "height: 100%"
    DOM.host (fun ctx ->
      let e = Monaco.editor.create(ctx, monacoEditorOptions initialText (themeName isLightTheme.Value))

      let resizerDisposable = ResizeObserver.getResizer(downcast ctx).Subscribe( fun _ -> e.layout())
      DOM.rafu( fun _ -> e.layout() )

      // let tabCode = Monaco.KeyCode.Tab |> int
      // let chord = KeyMod.chord( escKeyCode, tabCode)
      // let shortcut2 = KeyMod.CtrlCmd ||| (KeyMod.Shift |> int) ||| (Monaco.KeyCode.KeyS |> int)
      let escKeyCode = Monaco.KeyCode.Escape |> int
      let actionDisposable =
        e.addAction(
          {
            Monaco.Editor.IActionDescriptor.label = "Release keyboard focus"
            id = "wm-escape-tab"
            precondition = None
            keybindingContext = None
            contextMenuGroupId = Some "navigation"
            keybindings = ResizeArray([escKeyCode]) |> Some
            run = fun _ ->
              (Browser.Dom.document.activeElement :?> Browser.Types.HTMLElement).blur();
              U2.Case1 ()

            contextMenuOrder = Some 1.5
          }
        )

      let onDidChangeDisposable = e.onDidChangeModelContent.Invoke(onChange >> (fun () -> None))

      DOM.SutilNode.RegisterDisposable(ctx, resizerDisposable)
      DOM.SutilNode.RegisterDisposable(ctx, new MonacoDisposable(actionDisposable))
      DOM.SutilNode.RegisterDisposable(ctx, new MonacoDisposable(onDidChangeDisposable))

      isLightTheme |> Store.write setTheme
      e |> onCreated
    )
  ]

