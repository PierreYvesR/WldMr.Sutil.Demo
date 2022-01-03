namespace SutilExt

open Sutil

[<AutoOpen>]
module Operator =
  let (=/=|>) a b = a |> Store.distinct |> b
  let (|=/=>) a b = a |> Store.distinct |=> b
  let (.>=/=>) a b = a |> Store.distinct .> b


[<RequireQualifiedAccess>]
module Bind =
  let reactElement(obs, reactElementFun) =
    fun v -> DOM.host( fun el -> Fable.React.ReactDomBindings.ReactDom.render(reactElementFun v, el))
    |> bindElement obs


[<RequireQualifiedAccess>]
module Cmd =
  let private delay interval callback =
    let t = new System.Timers.Timer(interval, AutoReset = false)
    t.Elapsed.Add callback
    t.Enabled <- true
    t.Start()

  let ofMsgDelayed t msg =
    [ fun d -> delay t (fun _ -> d msg) ]

  let ups messageCtor =
    let handler dispatch =
      Interop.Window.addEventListener("mouseup", fun _ ->
        dispatch messageCtor)
    [ handler ]

  let move messageCtor =
    let handler dispatch =
      Interop.Window.addEventListener("mousemove", fun ev ->
        ev :?> Browser.Types.MouseEvent
        |> messageCtor
        |> dispatch)
    [ handler ]

[<RequireQualifiedAccess>]
module HtmlExt =
  let recDivClass classNames content =
    let rec inner cs elt =
      match cs with
      | [] -> elt
      | c :: rest -> inner rest (Html.div [ Attr.className (c : string); elt ])
    inner (List.rev classNames) (content |> DOM.fragment)


type IVirtualStore<'T> =
  interface
      inherit IStore<'T>
      abstract member UpdateCallback: ('T -> unit)
  end

type VirtualStore<'T> = IVirtualStore<'T>

[<RequireQualifiedAccess>]
module VirtualStore =
  open System

  type VirtualStore<'T, 'Model when 'T: equality>(
    name,
    initialModel: 'Model,
    obs: IObservable<'Model>,
    f: 'Model -> 'T,
    updateCallback: 'T -> unit
    ) as self
    =
    let mutable uid = 0
    let mutable _value = f initialModel
    let mutable _name = name
    let subscribers = Collections.Generic.Dictionary<_, IObserver<'T>>()
    let subDisposable = obs.Subscribe self.ProcessNewModel

    override _.ToString() = $"#VirtualStore={_value}"

    member _.Value = _value

    member this.Name with get() = _name and set (v) = _name <- v

    member _.UpdateCallback = updateCallback

    member _.Update(f: 'T -> 'T) =
        let newValue = f _value
        newValue |> updateCallback

    member private _.ProcessNewModel(model: 'Model) =
        let newValue = f model

        // Send every update. Use 'distinctUntilChanged' with fastEquals to get previous behaviour
        // Fable.Core.JS.console.log($"Update {model}, {_value} -> {newValue}")
        if newValue <> _value then
            _value <- newValue
            if subscribers.Count > 0 then
                subscribers.Values
                    |> Seq.iter (fun s -> s.OnNext(_value))

    member _.Subscribe(observer: IObserver<'T>): IDisposable =
        let id = uid
        uid <- uid + 1

        Logging.log "store" $"subscribe {id}"

        subscribers.Add(id, observer)

        // TODO: Is this the right way to report the model to the subscriber immediately?
        //Fable.Core.JS.setTimeout (fun _ -> observer.OnNext(model)) 0 |> ignore

        // Sutil depends on an immediate callback
        observer.OnNext(_value)

        Helpers.disposable <| fun () ->
            Logging.log "store" $"unsubscribe {id}"
            subscribers.Remove(id) |> ignore

    member this.Dispose() =
        subscribers.Values |> Seq.iter (fun x -> x.OnCompleted())
        subscribers.Clear()
        _value <- Unchecked.defaultof<_>
        subDisposable.Dispose()

    interface IVirtualStore<'T> with
        member this.Subscribe(observer: IObserver<'T>) = this.Subscribe(observer)
        member this.Value = this.Value
        member this.Update(f) = this.Update(f)
        member this.UpdateCallback = this.UpdateCallback
        member this.Debugger = {
            new IStoreDebugger with
                member _.Value = upcast this.Value
                member _.NumSubscribers = subscribers.Count }
        member this.Dispose() = this.Dispose()
        member this.Name with get() = this.Name and set (v:string) = this.Name <- v

  let ofStore (f: 'Model -> 'T) (callback: 'T -> unit) (store: Store<'Model>): IVirtualStore<'T> =
    new VirtualStore<'T, 'Model>($"Virtual-{store.Name}", store.Value, store, f, callback)

  let map (f: 'T -> 'U, invF: 'U -> 'T) (vStore: IVirtualStore<'T>): IVirtualStore<'U> =
      new VirtualStore<'U, _>($"Virtual-{vStore.Name}", vStore.Value, vStore, f, invF >> vStore.UpdateCallback)
