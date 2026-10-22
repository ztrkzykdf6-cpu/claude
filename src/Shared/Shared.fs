/// Domain types and the API contract shared by the server and the Fable client.
module Shared

open System

type User =
    { Id: int
      Username: string
      DisplayName: string
      Title: string
      Department: string
      Bio: string
      AvatarColor: string }

type Session = { Token: string; User: User }

type Group =
    { Id: int
      Name: string
      Description: string
      MemberCount: int
      IsMember: bool }

type Comment =
    { Id: int
      Author: User
      Body: string
      CreatedAt: DateTime }

type Post =
    { Id: int
      Author: User
      GroupId: int option
      GroupName: string option
      Body: string
      CreatedAt: DateTime
      LikeCount: int
      LikedByMe: bool
      Comments: Comment list }

type ChatMessage =
    { Id: int
      SenderId: int
      RecipientId: int
      Body: string
      SentAt: DateTime }

type Conversation =
    { Partner: User
      LastMessage: ChatMessage option }

/// Which posts to show: everything visible to the user, or a single group's posts.
type FeedQuery =
    | HomeFeed
    | GroupFeed of groupId: int

type RegisterRequest =
    { Username: string
      Password: string
      DisplayName: string
      Title: string
      Department: string }

type LoginRequest = { Username: string; Password: string }

type NewPost = { GroupId: int option; Body: string }

type NewGroup = { Name: string; Description: string }

type ProfileUpdate =
    { DisplayName: string
      Title: string
      Department: string
      Bio: string }

module Validation =
    let maxPostLength = 5000
    let maxMessageLength = 2000

    let username (value: string) =
        let ok =
            value.Length >= 3
            && value.Length <= 32
            && value |> Seq.forall (fun c -> Char.IsLetterOrDigit c || c = '_' || c = '.')

        if ok then
            Ok value
        else
            Error "Username must be 3-32 characters: letters, digits, '_' or '.'"

    let password (value: string) =
        if value.Length >= 8 then
            Ok value
        else
            Error "Password must be at least 8 characters"

    let required name (value: string) =
        if String.IsNullOrWhiteSpace value then
            Error(sprintf "%s is required" name)
        else
            Ok(value.Trim())

module Route =
    let builder typeName methodName = sprintf "/api/%s/%s" typeName methodName

/// Endpoints that work without a session.
type IAuthApi =
    { register: RegisterRequest -> Async<Result<Session, string>>
      login: LoginRequest -> Async<Result<Session, string>> }

/// Endpoints that require a bearer token; the server answers 401 without one.
type IWorkplaceApi =
    { me: unit -> Async<User>
      logout: unit -> Async<unit>
      getFeed: FeedQuery -> Async<Post list>
      createPost: NewPost -> Async<Result<Post, string>>
      deletePost: int -> Async<Result<unit, string>>
      toggleLike: int -> Async<Post>
      addComment: int -> string -> Async<Result<Post, string>>
      getGroups: unit -> Async<Group list>
      createGroup: NewGroup -> Async<Result<Group, string>>
      setMembership: int -> bool -> Async<Group>
      getPeople: string -> Async<User list>
      updateProfile: ProfileUpdate -> Async<Result<User, string>>
      getConversations: unit -> Async<Conversation list>
      /// Messages with a partner whose id is greater than the given id (0 = full history).
      getMessages: int -> int -> Async<ChatMessage list>
      sendMessage: int -> string -> Async<Result<ChatMessage, string>> }
