/// Implementations of the remoting contracts declared in Shared.
module Api

open Shared
open Db

/// Raised for missing resources; its message is passed through to the client.
exception NotFound of string

let private validate (checks: Result<string, string> list) =
    checks
    |> List.tryPick (function
        | Error e -> Some e
        | Ok _ -> None)
    |> function
        | Some e -> Error e
        | None -> Ok()

let private openSession (db: Store) (user: User) =
    let token = Auth.newToken ()
    db.CreateSession(Auth.hashToken token, user.Id)
    { Token = token; User = user }

let auth (db: Store) : IAuthApi =
    { register =
        fun req ->
            async {
                let req =
                    { req with
                        Username = req.Username.Trim()
                        DisplayName = req.DisplayName.Trim()
                        Title = req.Title.Trim()
                        Department = req.Department.Trim() }

                return
                    validate
                        [ Validation.username req.Username
                          Validation.password req.Password
                          Validation.required "Display name" req.DisplayName ]
                    |> Result.bind (fun () -> db.CreateUser(req, Auth.hashPassword req.Password))
                    |> Result.map (openSession db)
            }

      login =
        fun req ->
            async {
                return
                    match db.FindCredentials(req.Username.Trim()) with
                    | Some(user, hash) when Auth.verifyPassword req.Password hash -> Ok(openSession db user)
                    | _ -> Error "Incorrect username or password"
            } }

/// The API as seen by one signed-in user; built per request from the authenticated session.
/// `notify` pushes a newly sent chat message to the participants' live connections, `onPostCreated` lets
/// workflows react to new posts, and `botId` identifies the Workflow Bot account, which can't be messaged.
let workplace
    (db: Store)
    (notify: ChatMessage -> Async<unit>)
    (onPostCreated: Post -> Async<unit>)
    (botId: int)
    (me: User)
    (token: string)
    : IWorkplaceApi =
    let post postId =
        match db.GetPost(me.Id, postId) with
        | Some p -> p
        | None -> raise (NotFound "That post no longer exists")

    let group groupId =
        match db.GetGroup(me.Id, groupId) with
        | Some g -> g
        | None -> raise (NotFound "That group no longer exists")

    { me = fun () -> async { return db.GetUser me.Id |> Option.defaultValue me }

      logout = fun () -> async { db.DeleteSession(Auth.hashToken token) }

      getFeed = fun feed -> async { return db.GetFeed(me.Id, feed) }

      createPost =
        fun np ->
            async {
                let body = np.Body.Trim()

                if body = "" then
                    return Error "Write something before posting"
                elif body.Length > Validation.maxPostLength then
                    return Error $"Posts are limited to {Validation.maxPostLength} characters"
                else
                    match np.GroupId with
                    | Some g when not (db.IsMember(me.Id, g)) -> return Error "Join the group to post in it"
                    | groupId ->
                        let created = post (db.CreatePost(me.Id, groupId, body))
                        do! onPostCreated created
                        return Ok created
            }

      deletePost =
        fun postId ->
            async {
                return
                    match db.PostAuthor postId with
                    | None -> Error "That post no longer exists"
                    | Some author when author <> me.Id -> Error "You can only delete your own posts"
                    | Some _ -> Ok(db.DeletePost postId)
            }

      toggleLike =
        fun postId ->
            async {
                db.ToggleLike(me.Id, postId)
                return post postId
            }

      addComment =
        fun postId body ->
            async {
                let body = body.Trim()

                return
                    if body = "" then
                        Error "Comment is empty"
                    elif body.Length > Validation.maxPostLength then
                        Error "Comment is too long"
                    else
                        db.AddComment(me.Id, postId, body)
                        Ok(post postId)
            }

      getGroups = fun () -> async { return db.GetGroups me.Id }

      createGroup =
        fun g ->
            async {
                return
                    Validation.required "Group name" g.Name
                    |> Result.bind (fun name -> db.CreateGroup(me.Id, { g with Name = name; Description = g.Description.Trim() }))
                    |> Result.map group
            }

      setMembership =
        fun groupId isMember ->
            async {
                db.SetMembership(me.Id, groupId, isMember)
                return group groupId
            }

      getPeople = fun term -> async { return db.SearchUsers term }

      updateProfile =
        fun p ->
            async {
                let p =
                    { DisplayName = p.DisplayName.Trim()
                      Title = p.Title.Trim()
                      Department = p.Department.Trim()
                      Bio = p.Bio.Trim() }

                return
                    Validation.required "Display name" p.DisplayName
                    |> Result.map (fun _ ->
                        db.UpdateProfile(me.Id, p)
                        db.GetUser me.Id |> Option.get)
            }

      getConversations = fun () -> async { return db.GetConversations me.Id }

      getMessages = fun partnerId afterId -> async { return db.GetMessages(me.Id, partnerId, afterId) }

      sendMessage =
        fun recipientId body ->
            async {
                let body = body.Trim()

                if body = "" then
                    return Error "Message is empty"
                elif body.Length > Validation.maxMessageLength then
                    return Error $"Messages are limited to {Validation.maxMessageLength} characters"
                elif recipientId = me.Id then
                    return Error "You can't message yourself"
                elif recipientId = botId then
                    return Error "Workflow Bot only sends messages and can't read replies"
                elif (db.GetUser recipientId).IsNone then
                    return Error "That person no longer exists"
                else
                    let message = db.SendMessage(me.Id, recipientId, body)
                    do! notify message
                    return Ok message
            } }
