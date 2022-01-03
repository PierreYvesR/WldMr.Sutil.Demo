module Hotkeys


[<RequireQualifiedAccess>]
module Cmd =

  [<AutoOpen>]
  module private Internal =
    // by default HotkeysJS does not capture key bindings while in an 'editable' control
    let filterTrue () =
      HotkeysJs.hotkeys.filter <- (fun _ -> true)

  do filterTrue()


  let bindHotkey shortcut messageCtor =
    let handler dispatch =
      HotkeysJs.hotkeys.Invoke(key=shortcut,
        options=HotkeysJs.Options.empty,
        method= fun hkEvent ->
          dispatch messageCtor
          false |> Fable.Core.U2.Case2
      )
    [ handler ]
