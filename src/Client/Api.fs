/// Typed proxies for the server API, plus the stored session token.
module Api

open Fable.Core
open Fable.Remoting.Client
open Shared

let auth: IAuthApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<IAuthApi>

let workplace (token: string) : IWorkplaceApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.withAuthorizationHeader ("Bearer " + token)
    |> Remoting.buildProxy<IWorkplaceApi>

let isUnauthorized (ex: exn) =
    match ex with
    | :? ProxyRequestException as e -> e.StatusCode = 401
    | _ -> false

[<Emit("(() => { try { return JSON.parse($0).error } catch { return undefined } })()")>]
let private jsonError (_text: string) : string option = jsNative

/// A message fit to show the user for a failed call.
let errorMessage (ex: exn) =
    match ex with
    | :? ProxyRequestException as e -> jsonError e.ResponseText |> Option.defaultValue "The server couldn't complete that request"
    | _ ->
        Browser.Dom.console.error ex
        "Couldn't reach the server. Check your connection and try again."

module TokenStore =
    let private key = "workplace.token"

    [<Emit("(() => { try { return localStorage.getItem($0) ?? undefined } catch { return undefined } })()")>]
    let private getItem (_key: string) : string option = jsNative

    [<Emit("(() => { try { localStorage.setItem($0, $1) } catch {} })()")>]
    let private setItem (_key: string) (_value: string) : unit = jsNative

    [<Emit("(() => { try { localStorage.removeItem($0) } catch {} })()")>]
    let private removeItem (_key: string) : unit = jsNative

    let load () = getItem key
    let save token = setItem key token
    let clear () = removeItem key
