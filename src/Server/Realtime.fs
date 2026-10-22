/// SignalR hub that pushes new chat messages to the people in the conversation.
module Realtime

open System.Threading.Tasks
open Microsoft.AspNetCore.SignalR
open Newtonsoft.Json
open Fable.Remoting.Json
open Shared

let private groupFor (userId: int) = $"user:{userId}"

let private converter = FableJsonConverter()

/// Clients connect with `?access_token=<session token>`, since browsers can't set headers on WebSockets.
/// Each connection joins its user's group; the hub accepts no calls from clients.
type ChatHub(db: Db.Store) =
    inherit Hub()

    override this.OnConnectedAsync() =
        let user =
            match this.Context.GetHttpContext() with
            | null -> None
            | http ->
                match string http.Request.Query["access_token"] with
                | "" -> None
                | token -> db.UserForSession(Auth.hashToken token)

        match user with
        | Some u -> this.Groups.AddToGroupAsync(this.Context.ConnectionId, groupFor u.Id)
        | None ->
            this.Context.Abort()
            Task.CompletedTask

/// Sends a message to both participants' connections, serialized the way the Fable.Remoting client decodes it.
let notifyMessage (hub: IHubContext<ChatHub>) (message: ChatMessage) =
    async {
        let json = JsonConvert.SerializeObject(message, converter)

        do!
            hub.Clients.Groups([ groupFor message.SenderId; groupFor message.RecipientId ]).SendAsync("message", json)
            |> Async.AwaitTask
    }
