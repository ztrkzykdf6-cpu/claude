/// Live chat delivery over SignalR (`/hubs/chat`).
module Realtime

open System
open Fable.Core
open Fable.Core.JsInterop
open Fable.SimpleJson
open Shared

type private HubConnection =
    abstract on: methodName: string * handler: (string -> unit) -> unit
    [<Emit("$0.on($1, $2)")>]
    abstract onSignal: methodName: string * handler: (unit -> unit) -> unit
    abstract onreconnected: callback: (string -> unit) -> unit
    abstract start: unit -> JS.Promise<unit>
    abstract stop: unit -> JS.Promise<unit>

[<Import("HubConnectionBuilder", "@microsoft/signalr")>]
type private HubConnectionBuilder() =
    member _.withUrl(url: string, options: obj) : HubConnectionBuilder = jsNative
    member _.withAutomaticReconnect(policy: obj) : HubConnectionBuilder = jsNative
    member _.build() : HubConnection = jsNative

[<Emit("$0.then($1, $2)")>]
let private onSettled (_promise: JS.Promise<unit>) (_ok: unit -> unit) (_error: obj -> unit) : unit = jsNative

let private retryDelayMs (attempt: int) = if attempt < 3 then 1000 else 5000

/// Opens a connection for the signed-in user. `onMessage` gets every message sent to or by them, and
/// `onTasks` fires when their workflow tasks change. `onReconnected` fires after a dropped connection comes
/// back, so the caller can fetch what it missed. Disposing closes the connection.
let connect (token: string) (onMessage: ChatMessage -> unit) (onTasks: unit -> unit) (onReconnected: unit -> unit) : IDisposable =
    let connection =
        HubConnectionBuilder()
            .withUrl("/hubs/chat", createObj [ "accessTokenFactory" ==> fun () -> token ])
            // Keep retrying for as long as the page is open (the default policy gives up after four tries).
            .withAutomaticReconnect(createObj [ "nextRetryDelayInMilliseconds" ==> fun ctx -> retryDelayMs ctx?previousRetryCount ])
            .build ()

    connection.on ("message", fun json -> onMessage (Json.parseNativeAs<ChatMessage> json))
    connection.onSignal ("tasks", onTasks)
    connection.onreconnected (fun _ -> onReconnected ())

    let mutable stopped = false

    // Automatic reconnect only covers connections that were once up, so retry the first start by hand.
    let rec start attempt =
        onSettled
            (connection.start ())
            (fun () -> if attempt > 0 then onReconnected ())
            (fun _ ->
                if not stopped then
                    Browser.Dom.window.setTimeout ((fun () -> start (attempt + 1)), retryDelayMs attempt) |> ignore)

    start 0

    { new IDisposable with
        member _.Dispose() =
            stopped <- true
            connection.stop () |> ignore }
