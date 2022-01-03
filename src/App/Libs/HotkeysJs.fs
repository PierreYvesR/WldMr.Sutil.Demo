// hotkeys-js 3.8.7
// ts2fable 0.8.0
module rec HotkeysJs
open System
open Fable.Core
open Fable.Core.JS
open Browser.Types

let [<Import("default","hotkeys-js")>] hotkeys: Hotkeys = jsNative

type HotkeysEvent = {
  key: string
  method: KeyHandler
  mods: float[]
  scope: string
  shortcut: string
}

// type [<AllowNullLiteral>] KeyHandler =
//     [<Emit "$0($1...)">] abstract Invoke: keyboardEvent: KeyboardEvent * hotkeysEvent: HotkeysEvent -> U2<unit, bool>

type KeyHandler = (KeyboardEvent * HotkeysEvent -> U2<unit, bool>)


type Options = {
  scope: string option
  element: HTMLElement option
  keyup: bool option
  keydown: bool option
  splitKey: string option
}
  with
    static member empty: Options = {scope= None; element=None; keyup=None; keydown=None; splitKey= None}


type [<AllowNullLiteral>] Hotkeys =
    // [<Emit "$0($1...)">] abstract Invoke: key: string * method: KeyHandler -> unit
    // [<Emit "$0($1...)">] abstract Invoke: key: string * scope: string * method: KeyHandler -> unit
    [<Emit "$0($1...)">] abstract Invoke: key: string * options: Options * method: KeyHandler -> unit
    abstract shift: bool with get, set
    abstract ctrl: bool with get, set
    abstract alt: bool with get, set
    abstract option: bool with get, set
    abstract control: bool with get, set
    abstract cmd: bool with get, set
    abstract command: bool with get, set
    abstract setScope: scopeName: string -> unit
    abstract getScope: unit -> string
    abstract deleteScope: scopeName: string -> unit
    abstract noConflict: unit -> Hotkeys
    abstract unbind: ?key: string -> unit
    abstract unbind: key: string * scopeName: string -> unit
    abstract unbind: key: string * scopeName: string * method: KeyHandler -> unit
    abstract unbind: key: string * method: KeyHandler -> unit
    abstract isPressed: keyCode: float -> bool
    abstract isPressed: keyCode: string -> bool
    abstract getPressedKeyCodes: unit -> ResizeArray<float>
    abstract filter: (KeyboardEvent -> bool) with get, set
