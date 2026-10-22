/// Feliz views for every page.
module View

open System
open Feliz
open Shared
open State

let private initials (name: string) =
    name.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries)
    |> Array.truncate 2
    |> Array.map (fun part -> string (Char.ToUpper part[0]))
    |> String.concat ""

let private avatar (size: string) (user: User) =
    Html.div [
        prop.className ("avatar avatar-" + size)
        prop.style [ style.backgroundColor user.AvatarColor ]
        prop.title user.DisplayName
        prop.text (initials user.DisplayName)
    ]

let private months =
    [| "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun"; "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec" |]

// Formatted by hand: Fable supports only a subset of .NET date format strings.
let private formatDate (time: DateTime) =
    let t = time.ToLocalTime()
    sprintf "%s %d, %d" months[t.Month - 1] t.Day t.Year

let private formatDateTime (time: DateTime) =
    let t = time.ToLocalTime()
    sprintf "%s %02d:%02d" (formatDate time) t.Hour t.Minute

let private timeAgo (time: DateTime) =
    let elapsed = DateTime.UtcNow - time.ToUniversalTime()

    if elapsed.TotalMinutes < 1.0 then "just now"
    elif elapsed.TotalHours < 1.0 then sprintf "%dm" (int elapsed.TotalMinutes)
    elif elapsed.TotalDays < 1.0 then sprintf "%dh" (int elapsed.TotalHours)
    elif elapsed.TotalDays < 7.0 then sprintf "%dd" (int elapsed.TotalDays)
    else formatDate time

let private onEnter (action: unit -> unit) =
    prop.onKeyDown (fun (e: Browser.Types.KeyboardEvent) ->
        if e.key = "Enter" && not e.shiftKey then
            e.preventDefault ()
            action ())

let private button (className: string) (label: string) (onClick: unit -> unit) =
    Html.button [
        prop.type'.button
        prop.className className
        prop.text label
        prop.onClick (fun _ -> onClick ())
    ]

let private field (label: string) (input: ReactElement) =
    Html.label [ prop.className "field"; prop.children [ Html.span label; input ] ]

let private textInput (value: string) (placeholder: string) (onChange: string -> unit) =
    Html.input [
        prop.className "input"
        prop.value value
        prop.placeholder placeholder
        prop.onChange onChange
    ]

let private userLink (user: User) dispatch =
    Html.a [
        prop.className "user-link"
        prop.href "#"
        prop.text user.DisplayName
        prop.onClick (fun e ->
            e.preventDefault ()
            dispatch (Navigate(Profile user)))
    ]

// ---- Sign in / register -----------------------------------------------------

let authView (a: AuthModel) dispatch =
    let set f = fun value -> dispatch (SetAuthField(f, value))

    let tab mode (label: string) =
        Html.button [
            prop.type'.button
            prop.className (if a.Mode = mode then "tab active" else "tab")
            prop.text label
            prop.onClick (fun _ -> dispatch (SetAuthMode mode))
        ]

    Html.div [
        prop.className "auth-page"
        prop.children [
            Html.div [
                prop.className "auth-brand"
                prop.children [
                    Html.h1 "workplace"
                    Html.p "Connect your company: share updates, collaborate in groups and chat with colleagues."
                ]
            ]
            Html.form [
                prop.className "card auth-card"
                prop.onSubmit (fun e ->
                    e.preventDefault ()
                    dispatch SubmitAuth)
                prop.children [
                    Html.div [ prop.className "tabs"; prop.children [ tab SignIn "Sign in"; tab Register "Create account" ] ]
                    field "Username" (textInput a.Username "e.g. alex" (set Username))
                    field
                        "Password"
                        (Html.input [
                            prop.className "input"
                            prop.type'.password
                            prop.value a.Password
                            prop.onChange (set Password)
                        ])
                    if a.Mode = Register then
                        field "Full name" (textInput a.DisplayName "Alex Rivera" (set DisplayName))
                        field "Job title" (textInput a.Title "Product Manager" (set Title))
                        field "Department" (textInput a.Department "Engineering" (set Department))
                    match a.Error with
                    | Some e -> Html.p [ prop.className "error"; prop.text e ]
                    | None -> ()
                    Html.button [
                        prop.type'.submit
                        prop.className "btn btn-primary btn-block"
                        prop.disabled a.Busy
                        prop.text (
                            match a.Mode, a.Busy with
                            | _, true -> "Please wait…"
                            | SignIn, _ -> "Sign in"
                            | Register, _ -> "Create account"
                        )
                    ]
                    Html.p [
                        prop.className "hint"
                        prop.text "Demo accounts: alex, priya, sam, mei or jordan. Password: password123"
                    ]
                ]
            ]
        ]
    ]

// ---- Feed ---------------------------------------------------------------------

let private composer (m: AppModel) placeholder dispatch =
    Html.div [
        prop.className "card composer"
        prop.children [
            Html.div [
                prop.className "row"
                prop.children [
                    avatar "md" m.Me
                    Html.textarea [
                        prop.className "input composer-input"
                        prop.placeholder placeholder
                        prop.value m.Draft
                        prop.maxLength Validation.maxPostLength
                        prop.onChange (SetDraft >> dispatch)
                    ]
                ]
            ]
            Html.div [
                prop.className "composer-actions"
                prop.children [
                    Html.button [
                        prop.type'.button
                        prop.className "btn btn-primary"
                        prop.disabled (m.Draft.Trim() = "")
                        prop.text "Post"
                        prop.onClick (fun _ -> dispatch SubmitPost)
                    ]
                ]
            ]
        ]
    ]

let private postCard (m: AppModel) (post: Post) dispatch =
    let commentDraft = m.CommentDrafts.TryFind post.Id |> Option.defaultValue ""

    Html.article [
        prop.className "card post"
        prop.children [
            Html.header [
                prop.className "post-header"
                prop.children [
                    avatar "md" post.Author
                    Html.div [
                        prop.className "post-meta"
                        prop.children [
                            Html.div [
                                userLink post.Author dispatch
                                match post.GroupId, post.GroupName with
                                | Some g, Some name ->
                                    Html.span [ prop.className "muted"; prop.text " ▸ " ]
                                    Html.a [
                                        prop.className "group-link"
                                        prop.href "#"
                                        prop.text name
                                        prop.onClick (fun e ->
                                            e.preventDefault ()
                                            dispatch (Navigate(Feed(GroupFeed g))))
                                    ]
                                | _ -> ()
                            ]
                            Html.div [
                                prop.className "muted small"
                                prop.text (
                                    [ post.Author.Title; timeAgo post.CreatedAt ]
                                    |> List.filter (String.IsNullOrWhiteSpace >> not)
                                    |> String.concat " · "
                                )
                            ]
                        ]
                    ]
                    if post.Author.Id = m.Me.Id then
                        Html.button [
                            prop.type'.button
                            prop.className "btn-icon"
                            prop.title "Delete post"
                            prop.text "✕"
                            prop.onClick (fun _ ->
                                if Browser.Dom.window.confirm "Delete this post?" then
                                    dispatch (DeletePost post.Id))
                        ]
                ]
            ]
            Html.p [ prop.className "post-body"; prop.text post.Body ]
            Html.div [
                prop.className "post-stats muted small"
                prop.children [
                    Html.span (if post.LikeCount = 0 then "" else sprintf "👍 %d" post.LikeCount)
                    Html.span (
                        match post.Comments.Length with
                        | 0 -> ""
                        | 1 -> "1 comment"
                        | n -> sprintf "%d comments" n
                    )
                ]
            ]
            Html.div [
                prop.className "post-actions"
                prop.children [
                    Html.button [
                        prop.type'.button
                        prop.className (if post.LikedByMe then "action liked" else "action")
                        prop.text (if post.LikedByMe then "👍 Liked" else "👍 Like")
                        prop.onClick (fun _ -> dispatch (ToggleLike post.Id))
                    ]
                ]
            ]
            Html.div [
                prop.className "comments"
                prop.children [
                    for c in post.Comments do
                        Html.div [
                            prop.key c.Id
                            prop.className "comment"
                            prop.children [
                                avatar "sm" c.Author
                                Html.div [
                                    prop.className "comment-bubble"
                                    prop.children [
                                        userLink c.Author dispatch
                                        Html.p c.Body
                                        Html.span [ prop.className "muted small"; prop.text (timeAgo c.CreatedAt) ]
                                    ]
                                ]
                            ]
                        ]
                    Html.div [
                        prop.className "comment"
                        prop.children [
                            avatar "sm" m.Me
                            Html.input [
                                prop.className "input comment-input"
                                prop.placeholder "Write a comment…"
                                prop.value commentDraft
                                prop.onChange (fun (text: string) -> dispatch (SetCommentDraft(post.Id, text)))
                                onEnter (fun () -> dispatch (SubmitComment post.Id))
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]

let private posts (m: AppModel) dispatch =
    if m.PostsLoading then
        Html.p [ prop.className "empty"; prop.text "Loading posts…" ]
    elif m.Posts.IsEmpty then
        Html.p [ prop.className "empty"; prop.text "No posts yet. Start the conversation!" ]
    else
        Html.div [ for p in m.Posts -> Html.div [ prop.key p.Id; prop.children [ postCard m p dispatch ] ] ]

let private membershipButton (g: Group) dispatch =
    if g.IsMember then
        button "btn" "Leave" (fun () -> dispatch (SetMembership(g.Id, false)))
    else
        button "btn btn-primary" "Join" (fun () -> dispatch (SetMembership(g.Id, true)))

let private feedPage (m: AppModel) feed dispatch =
    match feed with
    | HomeFeed ->
        Html.div [
            Html.h2 [ prop.className "page-title"; prop.text "Home" ]
            composer m (sprintf "What's on your mind, %s?" (m.Me.DisplayName.Split(' ')[0])) dispatch
            posts m dispatch
        ]
    | GroupFeed groupId ->
        let group = m.Groups |> List.tryFind (fun g -> g.Id = groupId)

        Html.div [
            match group with
            | Some g ->
                Html.div [
                    prop.className "card group-banner"
                    prop.children [
                        Html.div [
                            Html.h2 g.Name
                            Html.p [ prop.className "muted"; prop.text g.Description ]
                            Html.p [
                                prop.className "muted small"
                                prop.text (sprintf "%d member%s" g.MemberCount (if g.MemberCount = 1 then "" else "s"))
                            ]
                        ]
                        membershipButton g dispatch
                    ]
                ]

                if g.IsMember then
                    composer m (sprintf "Post in %s…" g.Name) dispatch
            | None -> ()
            posts m dispatch
        ]

// ---- Groups -------------------------------------------------------------------

let private groupsPage (m: AppModel) dispatch =
    Html.div [
        Html.h2 [ prop.className "page-title"; prop.text "Groups" ]
        Html.form [
            prop.className "card stack"
            prop.onSubmit (fun e ->
                e.preventDefault ()
                dispatch SubmitGroup)
            prop.children [
                Html.h3 "Create a group"
                textInput m.NewGroup.Name "Group name" (SetNewGroupName >> dispatch)
                textInput m.NewGroup.Description "What is this group about?" (SetNewGroupDescription >> dispatch)
                Html.div [
                    Html.button [
                        prop.type'.submit
                        prop.className "btn btn-primary"
                        prop.disabled (m.NewGroup.Name.Trim() = "")
                        prop.text "Create group"
                    ]
                ]
            ]
        ]
        Html.div [
            prop.className "grid"
            prop.children [
                for g in m.Groups ->
                    Html.div [
                        prop.key g.Id
                        prop.className "card group-card"
                        prop.children [
                            Html.a [
                                prop.href "#"
                                prop.className "group-card-name"
                                prop.text g.Name
                                prop.onClick (fun e ->
                                    e.preventDefault ()
                                    dispatch (Navigate(Feed(GroupFeed g.Id))))
                            ]
                            Html.p [ prop.className "muted"; prop.text g.Description ]
                            Html.div [
                                prop.className "row spread"
                                prop.children [
                                    Html.span [ prop.className "muted small"; prop.text (sprintf "%d members" g.MemberCount) ]
                                    membershipButton g dispatch
                                ]
                            ]
                        ]
                    ]
            ]
        ]
    ]

// ---- People & profiles ------------------------------------------------------

let private peoplePage (m: AppModel) dispatch =
    Html.div [
        Html.h2 [ prop.className "page-title"; prop.text "People" ]
        Html.input [
            prop.className "input search"
            prop.placeholder "Search by name, title or department"
            prop.value m.PeopleSearch
            prop.onChange (SetPeopleSearch >> dispatch)
        ]
        Html.div [
            prop.className "grid"
            prop.children [
                for u in m.People ->
                    Html.div [
                        prop.key u.Id
                        prop.className "card person-card"
                        prop.children [
                            avatar "lg" u
                            userLink u dispatch
                            Html.span [ prop.className "muted small"; prop.text u.Title ]
                            Html.span [ prop.className "muted small"; prop.text u.Department ]
                            if u.Id <> m.Me.Id then
                                button "btn" "Message" (fun () -> dispatch (Navigate(Chat(Some u))))
                        ]
                    ]
            ]
        ]
    ]

let private profilePage (m: AppModel) (user: User) dispatch =
    let user = if user.Id = m.Me.Id then m.Me else user

    Html.div [
        prop.className "card profile"
        prop.children [
            avatar "xl" user
            match m.ProfileDraft with
            | Some d when user.Id = m.Me.Id ->
                Html.form [
                    prop.className "stack profile-form"
                    prop.onSubmit (fun e ->
                        e.preventDefault ()
                        dispatch SaveProfile)
                    prop.children [
                        field "Full name" (textInput d.DisplayName "" (fun v -> dispatch (SetProfileDraft { d with DisplayName = v })))
                        field "Job title" (textInput d.Title "" (fun v -> dispatch (SetProfileDraft { d with Title = v })))
                        field "Department" (textInput d.Department "" (fun v -> dispatch (SetProfileDraft { d with Department = v })))
                        field
                            "About"
                            (Html.textarea [
                                prop.className "input"
                                prop.value d.Bio
                                prop.onChange (fun (v: string) -> dispatch (SetProfileDraft { d with Bio = v }))
                            ])
                        Html.div [
                            prop.className "row"
                            prop.children [
                                Html.button [ prop.type'.submit; prop.className "btn btn-primary"; prop.text "Save" ]
                                button "btn" "Cancel" (fun () -> dispatch CancelProfileEdit)
                            ]
                        ]
                    ]
                ]
            | _ ->
                Html.h2 user.DisplayName
                Html.p [ prop.className "muted"; prop.text ([ user.Title; user.Department ] |> List.filter ((<>) "") |> String.concat " · ") ]
                Html.p [ prop.className "muted small"; prop.text ("@" + user.Username) ]
                if user.Bio <> "" then
                    Html.p [ prop.className "bio"; prop.text user.Bio ]
                if user.Id = m.Me.Id then
                    button "btn" "Edit profile" (fun () -> dispatch EditProfile)
                else
                    button "btn btn-primary" "Message" (fun () -> dispatch (Navigate(Chat(Some user))))
        ]
    ]

// ---- Chat ---------------------------------------------------------------------

let private chatPage (m: AppModel) (partner: User option) dispatch =
    Html.div [
        prop.className "card chat"
        prop.children [
            Html.aside [
                prop.className "chat-list"
                prop.children [
                    Html.h3 "Chats"
                    if m.Conversations.IsEmpty then
                        Html.p [ prop.className "muted small"; prop.text "No chats yet. Find someone in People to start one." ]
                    for c in m.Conversations do
                        Html.button [
                            prop.key c.Partner.Id
                            prop.type'.button
                            prop.className (
                                if partner |> Option.exists (fun p -> p.Id = c.Partner.Id) then "chat-item active" else "chat-item"
                            )
                            prop.onClick (fun _ -> dispatch (Navigate(Chat(Some c.Partner))))
                            prop.children [
                                avatar "md" c.Partner
                                Html.div [
                                    prop.className "chat-item-text"
                                    prop.children [
                                        Html.strong c.Partner.DisplayName
                                        match c.LastMessage with
                                        | Some msg ->
                                            Html.span [
                                                prop.className "muted small ellipsis"
                                                prop.text ((if msg.SenderId = m.Me.Id then "You: " else "") + msg.Body)
                                            ]
                                        | None -> ()
                                    ]
                                ]
                            ]
                        ]
                ]
            ]
            match partner with
            | None ->
                Html.div [ prop.className "chat-thread empty"; prop.text "Select a chat to start messaging" ]
            | Some p ->
                Html.section [
                    prop.className "chat-thread"
                    prop.children [
                        Html.header [
                            prop.className "chat-header"
                            prop.children [
                                avatar "md" p
                                Html.div [ userLink p dispatch; Html.div [ prop.className "muted small"; prop.text p.Title ] ]
                            ]
                        ]
                        Html.div [
                            prop.className "messages"
                            // Keep the newest message in view as the thread grows.
                            prop.ref (fun (el: Browser.Types.Element) ->
                                if not (isNull el) then
                                    el.scrollTop <- el.scrollHeight)
                            prop.children [
                                for msg in m.Messages ->
                                    Html.div [
                                        prop.key msg.Id
                                        prop.className (if msg.SenderId = m.Me.Id then "bubble mine" else "bubble")
                                        prop.title (formatDateTime msg.SentAt)
                                        prop.text msg.Body
                                    ]
                            ]
                        ]
                        Html.div [
                            prop.className "chat-compose"
                            prop.children [
                                Html.input [
                                    prop.className "input"
                                    prop.placeholder (sprintf "Message %s" p.DisplayName)
                                    prop.value m.MessageDraft
                                    prop.maxLength Validation.maxMessageLength
                                    prop.onChange (SetMessageDraft >> dispatch)
                                    onEnter (fun () -> dispatch SendMessage)
                                ]
                                button "btn btn-primary" "Send" (fun () -> dispatch SendMessage)
                            ]
                        ]
                    ]
                ]
        ]
    ]

// ---- Shell ----------------------------------------------------------------------

let private navItem (icon: string) (label: string) (page: Page) (isActive: bool) dispatch =
    Html.button [
        prop.type'.button
        prop.className (if isActive then "nav-item active" else "nav-item")
        prop.onClick (fun _ -> dispatch (Navigate page))
        prop.children [ Html.span [ prop.className "nav-icon"; prop.text icon ]; Html.span label ]
    ]

let private appView (m: AppModel) dispatch =
    let isGroupPage id =
        match m.Page with
        | Feed(GroupFeed g) -> g = id
        | _ -> false

    Html.div [
        prop.className "app"
        prop.children [
            Html.header [
                prop.className "topbar"
                prop.children [
                    Html.span [ prop.className "logo"; prop.text "workplace" ]
                    Html.div [
                        prop.className "topbar-user"
                        prop.children [
                            Html.button [
                                prop.type'.button
                                prop.className "me"
                                prop.onClick (fun _ -> dispatch (Navigate(Profile m.Me)))
                                prop.children [ avatar "sm" m.Me; Html.span m.Me.DisplayName ]
                            ]
                            button "btn" "Sign out" (fun () -> dispatch SignOut)
                        ]
                    ]
                ]
            ]
            Html.div [
                prop.className "layout"
                prop.children [
                    Html.nav [
                        prop.className "sidebar"
                        prop.children [
                            navItem "🏠" "Home" (Feed HomeFeed) (m.Page = Feed HomeFeed) dispatch
                            navItem "👥" "Groups" Groups (m.Page = Groups) dispatch
                            navItem "💬" "Chat" (Chat None) (match m.Page with Chat _ -> true | _ -> false) dispatch
                            navItem "📇" "People" People (m.Page = People) dispatch
                            Html.h4 [ prop.className "sidebar-heading"; prop.text "Your groups" ]
                            for g in m.Groups |> List.filter (fun g -> g.IsMember) do
                                Html.button [
                                    prop.key g.Id
                                    prop.type'.button
                                    prop.className (if isGroupPage g.Id then "nav-item small active" else "nav-item small")
                                    prop.text g.Name
                                    prop.onClick (fun _ -> dispatch (Navigate(Feed(GroupFeed g.Id))))
                                ]
                        ]
                    ]
                    Html.main [
                        prop.className "content"
                        prop.children [
                            match m.Notice with
                            | Some text ->
                                Html.div [
                                    prop.className "notice"
                                    prop.children [
                                        Html.span text
                                        Html.button [
                                            prop.type'.button
                                            prop.className "btn-icon"
                                            prop.text "✕"
                                            prop.onClick (fun _ -> dispatch DismissNotice)
                                        ]
                                    ]
                                ]
                            | None -> ()
                            match m.Page with
                            | Feed feed -> feedPage m feed dispatch
                            | Groups -> groupsPage m dispatch
                            | People -> peoplePage m dispatch
                            | Chat partner -> chatPage m partner dispatch
                            | Profile user -> profilePage m user dispatch
                        ]
                    ]
                ]
            ]
        ]
    ]

let view (model: Model) dispatch =
    match model with
    | Starting -> Html.div [ prop.className "splash"; prop.text "workplace" ]
    | Auth a -> authView a dispatch
    | App m -> appView m dispatch
