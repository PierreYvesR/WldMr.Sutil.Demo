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


module Observable =
  open System
  let pairwiseOpt (source: IObservable<'T>) : IObservable<'T option * 'T> =
    { new IObservable<_> with
        member x.Subscribe(observer) =
          let mutable lastArgs = None
          source.Subscribe
            { new Observable.BasicObserver<'T>() with
                member _.Next(args2) =
                  observer.OnNext (lastArgs,args2)
                  lastArgs <- Some args2
                member _.Error(e) = observer.OnError(e)
                member _.Completed() = observer.OnCompleted() } }

module SutilExt =
  open System
  open Sutil.DOM

  let bindVirtual<'T>  (store : IObservable<'T>) (element: 'T -> SutilElement) = nodeFactory <| fun ctx ->
    let mutable node = EmptyNode
    let group = SutilNode.MakeGroup("bind",ctx.Parent,ctx.Previous)
    let bindNode = GroupNode group

    log($"bind: {group.Id} ctx={ctx.Action} prev={ctx.Previous}")
    ctx.AddChild bindNode

    let bindCtx = { ctx with Parent = bindNode }
    let disposable = store |> Store.subscribe (fun next ->
      try
        log($"bind: rebuild {group.Id} with {next}")
        node <- build (next |> element) (bindCtx |> ContextHelpers.withReplace (node,group.NextDomNode))
      with
      | x -> Logging.error $"Exception in bindo: {x.Message} parent {ctx.Parent} node {node.ToString()} node.Parent "
    )
    group.SetDispose ( fun () ->
      log($"dispose: Bind.el: {group}")
      disposable.Dispose())

    sutilResult bindNode

module Attr =
  open Sutil.Attr
  let onFocusout (fn : Browser.Types.FocusEvent -> unit) options =
    on "focusout" (unbox fn) options

  let onDblclick fn options =
    on "dblclick" fn options

  open DOM
  open Fable.Core.JsInterop
  let focus() : SutilElement =
    nodeFactory <| fun ctx ->
      let e = ctx.ParentElement
      DOM.rafu (fun _ ->
        e.focus()
        e?setSelectionRange(99999,99999)
      )
      unitResult(ctx, "focus")

  let autofocusWhenTrue b : SutilElement =
    nodeFactory <| fun ctx ->
      if b then
        let e = ctx.ParentElement
        DOM.rafu (fun _ ->
          e.focus()
          e?setSelectionRange(99999,99999)
        )
      unitResult(ctx, "autofocusWhenTrue")

  let toggleClassName(classesWhenTrue, classesWhenFalse) b: SutilElement =
    if b then
      nodeFactory ( fun ctx ->
        ctx.ParentElement |> removeFromClasslist classesWhenFalse
        ctx.ParentElement |> addToClasslist classesWhenTrue
        unitResult(ctx, "toggleClassName")
      )
    else
      nodeFactory ( fun ctx ->
        ctx.ParentElement |> removeFromClasslist classesWhenTrue
        ctx.ParentElement |> addToClasslist classesWhenFalse
        unitResult(ctx, "toggleClassName")
      )

module Iter =
  open DOM

  let eval (f: _ -> unit) v: SutilElement =
    nodeFactory (fun ctx -> f v; unitResult(ctx, "Iter.eval"))
