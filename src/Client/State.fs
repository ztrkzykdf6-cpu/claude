/// Elmish model, messages and update logic.
module State

open System
open Elmish
open Shared
open Workflows

type WorkflowTab =
    | AllWorkflows
    | MyTasks

type Page =
    | Feed of FeedQuery
    | Groups
    | People
    | Chat of partner: User option
    | Profile of User
    | WorkflowsPage of WorkflowTab
    | WorkflowEditor
    | WorkflowResultsPage of workflowId: int

type AuthMode =
    | SignIn
    | Register

type AuthModel =
    { Mode: AuthMode
      Username: string
      Password: string
      DisplayName: string
      Title: string
      Department: string
      Busy: bool
      Error: string option }

type AppModel =
    { Api: IWorkplaceApi
      WorkflowApi: IWorkflowApi
      Token: string
      Me: User
      Page: Page
      Posts: Post list
      PostsLoading: bool
      Draft: string
      CommentDrafts: Map<int, string>
      Groups: Group list
      NewGroup: NewGroup
      PeopleSearch: string
      People: User list
      Conversations: Conversation list
      Messages: ChatMessage list
      MessageDraft: string
      /// Chat partners with messages that arrived while their thread wasn't open.
      Unread: Set<int>
      ProfileDraft: ProfileUpdate option
      Workflows: Workflow list
      /// Paused workflow runs waiting on the current user.
      Tasks: PendingTask list
      Results: WorkflowResults option
      Editor: WorkflowDraft
      Saving: bool
      /// Form answers being typed, by run id and field key.
      TaskAnswers: Map<int, Map<string, string>>
      TaskComments: Map<int, string>
      /// Everyone, for the builder's person pickers.
      Directory: User list
      Notice: string option }

type Model =
    | Starting
    | Auth of AuthModel
    | App of AppModel

type AuthField =
    | Username
    | Password
    | DisplayName
    | Title
    | Department

type Msg =
    // Session
    | SessionRestored of token: string * User
    | SessionInvalid
    | SetAuthMode of AuthMode
    | SetAuthField of AuthField * string
    | SubmitAuth
    | AuthResult of Result<Session, string>
    | SignOut
    | Failed of exn
    | DismissNotice
    // Navigation
    | Navigate of Page
    // Feed
    | PostsLoaded of FeedQuery * Post list
    | SetDraft of string
    | SubmitPost
    | PostCreated of Result<Post, string>
    | PostUpdated of Post
    | ToggleLike of postId: int
    | DeletePost of postId: int
    | PostDeleted of postId: int * Result<unit, string>
    | SetCommentDraft of postId: int * string
    | SubmitComment of postId: int
    | CommentAdded of postId: int * Result<Post, string>
    // Groups
    | GroupsLoaded of Group list
    | SetNewGroupName of string
    | SetNewGroupDescription of string
    | SubmitGroup
    | GroupCreated of Result<Group, string>
    | SetMembership of groupId: int * bool
    | GroupUpdated of Group
    // People & profile
    | SetPeopleSearch of string
    | PeopleLoaded of term: string * User list
    | EditProfile
    | SetProfileDraft of ProfileUpdate
    | CancelProfileEdit
    | SaveProfile
    | ProfileSaved of Result<User, string>
    // Chat
    | ConversationsLoaded of Conversation list
    | MessagesLoaded of partnerId: int * ChatMessage list
    | MessageReceived of ChatMessage
    | CatchUpChat
    | SetMessageDraft of string
    | SendMessage
    | MessageSent of Result<ChatMessage, string>
    // Workflows
    | WorkflowsLoaded of Workflow list
    | TasksLoaded of PendingTask list
    | TasksChanged
    | DirectoryLoaded of User list
    | NewWorkflow
    | OpenWorkflowEditor of Workflow
    | EditWorkflow of WorkflowDraft
    | SaveWorkflow
    | WorkflowSaved of Result<Workflow, string>
    | DeleteWorkflow of workflowId: int
    | WorkflowDeleted of workflowId: int * Result<unit, string>
    | SetWorkflowEnabled of workflowId: int * bool
    | WorkflowUpdated of Result<Workflow, string>
    | RunWorkflow of workflowId: int
    | WorkflowStarted of Result<WorkflowRun, string>
    | ResultsLoaded of workflowId: int * Result<WorkflowResults, string>
    | SetTaskAnswer of runId: int * key: string * value: string
    | SetTaskComment of runId: int * string
    | SubmitTask of runId: int
    | DecideTask of runId: int * approve: bool
    | TaskDone of runId: int * Result<unit, string>

let private emptyAuth =
    { Mode = SignIn
      Username = ""
      Password = ""
      DisplayName = ""
      Title = ""
      Department = ""
      Busy = false
      Error = None }

let private emptyDraft =
    { Id = 0
      GroupId = 0
      Name = ""
      Description = ""
      Instructions = ""
      ResultsVisibility = GroupMembers
      Trigger = Manual
      Steps = [] }

let private appModel token me =
    { Api = Api.workplace token
      WorkflowApi = Api.workflows token
      Token = token
      Me = me
      Page = Feed HomeFeed
      Posts = []
      PostsLoading = true
      Draft = ""
      CommentDrafts = Map.empty
      Groups = []
      NewGroup = { Name = ""; Description = "" }
      PeopleSearch = ""
      People = []
      Conversations = []
      Messages = []
      MessageDraft = ""
      Unread = Set.empty
      ProfileDraft = None
      Workflows = []
      Tasks = []
      Results = None
      Editor = emptyDraft
      Saving = false
      TaskAnswers = Map.empty
      TaskComments = Map.empty
      Directory = []
      Notice = None }

let private call fn arg onOk = Cmd.OfAsync.either fn arg onOk Failed

let private loadFeed (m: AppModel) feed = call m.Api.getFeed feed (fun posts -> PostsLoaded(feed, posts))
let private loadGroups (m: AppModel) = call m.Api.getGroups () GroupsLoaded
let private loadConversations (m: AppModel) = call m.Api.getConversations () ConversationsLoaded

let private loadTasks (m: AppModel) = call m.WorkflowApi.getTasks () TasksLoaded
let private loadWorkflows (m: AppModel) = call m.WorkflowApi.getWorkflows () WorkflowsLoaded

let private loadMessages (m: AppModel) (partner: User) afterId =
    Cmd.OfAsync.either (fun () -> m.Api.getMessages partner.Id afterId) () (fun msgs -> MessagesLoaded(partner.Id, msgs)) Failed

let init () : Model * Cmd<Msg> =
    match Api.TokenStore.load () with
    | Some token ->
        let api = Api.workplace token
        Starting, Cmd.OfAsync.either api.me () (fun me -> SessionRestored(token, me)) (fun _ -> SessionInvalid)
    | None -> Auth emptyAuth, Cmd.none

let private startApp token me =
    let m = appModel token me
    App m, Cmd.batch [ loadFeed m HomeFeed; loadGroups m; loadTasks m ]

let private replacePost (post: Post) (posts: Post list) =
    posts |> List.map (fun p -> if p.Id = post.Id then post else p)

let private replaceGroup (group: Group) (groups: Group list) =
    if groups |> List.exists (fun g -> g.Id = group.Id) then
        groups |> List.map (fun g -> if g.Id = group.Id then group else g)
    else
        groups @ [ group ] |> List.sortBy (fun g -> g.Name.ToLowerInvariant())

/// Adds messages to the open thread if it is the one with `partnerId`, skipping any already shown.
let private appendMessages partnerId (incoming: ChatMessage list) (m: AppModel) =
    match m.Page with
    | Chat(Some partner) when partner.Id = partnerId ->
        let known = m.Messages |> List.map (fun x -> x.Id) |> Set.ofList
        let fresh = incoming |> List.filter (fun x -> not (known.Contains x.Id))
        { m with Messages = m.Messages @ fresh |> List.sortBy (fun x -> x.Id) }
    | _ -> m

/// Reloads what the current page shows after a workflow run changes it (run counts, results).
let private refreshPage (m: AppModel) =
    match m.Page with
    | Feed feed -> loadFeed m feed
    | WorkflowsPage _ -> loadWorkflows m
    | WorkflowResultsPage id -> Cmd.OfAsync.either (fun () -> m.WorkflowApi.getResults id) () (fun r -> ResultsLoaded(id, r)) Failed
    | _ -> Cmd.none

let private notice text m = { m with Notice = Some text }, Cmd.none

let private navigate page (m: AppModel) =
    let m = { m with Page = page; Notice = None; ProfileDraft = None }

    match page with
    | Feed feed -> { m with Posts = []; PostsLoading = true }, Cmd.batch [ loadFeed m feed; loadGroups m; loadWorkflows m ]
    | Groups -> m, loadGroups m
    | People -> m, call m.Api.getPeople m.PeopleSearch (fun people -> PeopleLoaded(m.PeopleSearch, people))
    | Chat partner ->
        let unread =
            match partner with
            | Some p -> m.Unread.Remove p.Id
            | None -> m.Unread

        let m = { m with Messages = []; MessageDraft = ""; Unread = unread }
        let thread = partner |> Option.map (fun p -> loadMessages m p 0) |> Option.defaultValue Cmd.none
        m, Cmd.batch [ loadConversations m; thread ]
    | Profile _ -> m, Cmd.none
    | WorkflowsPage _ -> m, Cmd.batch [ loadWorkflows m; loadTasks m; loadGroups m ]
    | WorkflowEditor -> m, Cmd.batch [ call m.Api.getPeople "" DirectoryLoaded; loadGroups m ]
    | WorkflowResultsPage id ->
        { m with Results = None },
        Cmd.OfAsync.either (fun () -> m.WorkflowApi.getResults id) () (fun r -> ResultsLoaded(id, r)) Failed

let private updateAuth msg (a: AuthModel) : Model * Cmd<Msg> =
    match msg with
    | SetAuthMode mode -> Auth { a with Mode = mode; Error = None }, Cmd.none
    | SetAuthField(field, value) ->
        let a =
            match field with
            | Username -> { a with Username = value }
            | Password -> { a with Password = value }
            | DisplayName -> { a with DisplayName = value }
            | Title -> { a with Title = value }
            | Department -> { a with Department = value }

        Auth a, Cmd.none
    | SubmitAuth when not a.Busy ->
        let request =
            match a.Mode with
            | SignIn -> Api.auth.login { Username = a.Username; Password = a.Password }
            | Register ->
                Api.auth.register
                    { Username = a.Username
                      Password = a.Password
                      DisplayName = a.DisplayName
                      Title = a.Title
                      Department = a.Department }

        Auth { a with Busy = true; Error = None }, Cmd.OfAsync.either (fun () -> request) () AuthResult Failed
    | AuthResult(Ok session) ->
        Api.TokenStore.save session.Token
        startApp session.Token session.User
    | AuthResult(Error e) -> Auth { a with Busy = false; Error = Some e }, Cmd.none
    | Failed ex -> Auth { a with Busy = false; Error = Some(Api.errorMessage ex) }, Cmd.none
    | _ -> Auth a, Cmd.none

let private updateApp msg (m: AppModel) : Model * Cmd<Msg> =
    let stay (m, cmd) = App m, cmd

    match msg with
    | SignOut ->
        Api.TokenStore.clear ()
        Auth emptyAuth, Cmd.OfAsync.attempt m.Api.logout () (fun _ -> DismissNotice)
    | Failed ex when Api.isUnauthorized ex ->
        Api.TokenStore.clear ()
        Auth { emptyAuth with Error = Some "Your session has ended. Please sign in again." }, Cmd.none
    | Failed ex -> notice (Api.errorMessage ex) m |> stay
    | DismissNotice -> App { m with Notice = None }, Cmd.none
    | Navigate page -> navigate page m |> stay

    | PostsLoaded(feed, posts) when m.Page = Feed feed ->
        // Keep posts the user created while this feed was still loading.
        let newest = posts |> List.tryHead |> Option.map (fun p -> p.Id) |> Option.defaultValue 0
        let pending = m.Posts |> List.filter (fun p -> p.Id > newest)
        App { m with Posts = pending @ posts; PostsLoading = false }, Cmd.none
    | SetDraft text -> App { m with Draft = text }, Cmd.none
    | SubmitPost when m.Draft.Trim() <> "" ->
        let groupId =
            match m.Page with
            | Feed(GroupFeed g) -> Some g
            | _ -> None

        App m, call m.Api.createPost { GroupId = groupId; Body = m.Draft } PostCreated
    | PostCreated(Ok post) -> App { m with Draft = ""; Posts = post :: m.Posts }, Cmd.none
    | PostUpdated post -> App { m with Posts = replacePost post m.Posts }, Cmd.none
    | ToggleLike postId -> App m, call m.Api.toggleLike postId PostUpdated
    | DeletePost postId -> App m, call m.Api.deletePost postId (fun r -> PostDeleted(postId, r))
    | PostDeleted(postId, Ok()) -> App { m with Posts = m.Posts |> List.filter (fun p -> p.Id <> postId) }, Cmd.none
    | SetCommentDraft(postId, text) -> App { m with CommentDrafts = m.CommentDrafts.Add(postId, text) }, Cmd.none
    | SubmitComment postId ->
        match m.CommentDrafts.TryFind postId with
        | Some text when text.Trim() <> "" ->
            App m, Cmd.OfAsync.either (fun () -> m.Api.addComment postId text) () (fun r -> CommentAdded(postId, r)) Failed
        | _ -> App m, Cmd.none
    | CommentAdded(postId, Ok post) ->
        App
            { m with
                Posts = replacePost post m.Posts
                CommentDrafts = m.CommentDrafts.Remove postId },
        Cmd.none

    | GroupsLoaded groups -> App { m with Groups = groups }, Cmd.none
    | SetNewGroupName name -> App { m with NewGroup = { m.NewGroup with Name = name } }, Cmd.none
    | SetNewGroupDescription d -> App { m with NewGroup = { m.NewGroup with Description = d } }, Cmd.none
    | SubmitGroup when m.NewGroup.Name.Trim() <> "" -> App m, call m.Api.createGroup m.NewGroup GroupCreated
    | GroupCreated(Ok group) ->
        App
            { m with
                Groups = replaceGroup group m.Groups
                NewGroup = { Name = ""; Description = "" } },
        Cmd.ofMsg (Navigate(Feed(GroupFeed group.Id)))
    | SetMembership(groupId, isMember) ->
        App m, Cmd.OfAsync.either (fun () -> m.Api.setMembership groupId isMember) () GroupUpdated Failed
    | GroupUpdated group -> App { m with Groups = replaceGroup group m.Groups }, Cmd.none

    | SetPeopleSearch term -> App { m with PeopleSearch = term }, call m.Api.getPeople term (fun people -> PeopleLoaded(term, people))
    | PeopleLoaded(term, people) when term = m.PeopleSearch -> App { m with People = people }, Cmd.none
    | EditProfile ->
        App
            { m with
                ProfileDraft =
                    Some
                        { DisplayName = m.Me.DisplayName
                          Title = m.Me.Title
                          Department = m.Me.Department
                          Bio = m.Me.Bio } },
        Cmd.none
    | SetProfileDraft draft -> App { m with ProfileDraft = Some draft }, Cmd.none
    | CancelProfileEdit -> App { m with ProfileDraft = None }, Cmd.none
    | SaveProfile ->
        match m.ProfileDraft with
        | Some draft -> App m, call m.Api.updateProfile draft ProfileSaved
        | None -> App m, Cmd.none
    | ProfileSaved(Ok me) -> App { m with Me = me; ProfileDraft = None; Page = Profile me }, Cmd.none

    | ConversationsLoaded conversations -> App { m with Conversations = conversations }, Cmd.none
    | MessagesLoaded(partnerId, incoming) -> App(appendMessages partnerId incoming m), Cmd.none
    | MessageReceived message ->
        let partnerId = if message.SenderId = m.Me.Id then message.RecipientId else message.SenderId

        let m =
            match m.Page with
            | Chat(Some partner) when partner.Id = partnerId -> appendMessages partnerId [ message ] m
            | _ when message.SenderId <> m.Me.Id -> { m with Unread = m.Unread.Add partnerId }
            | _ -> m

        match m.Page with
        | Chat _ -> App m, loadConversations m
        | _ -> App m, Cmd.none
    | CatchUpChat ->
        match m.Page with
        | Chat partner ->
            let lastId = m.Messages |> List.tryLast |> Option.map (fun x -> x.Id) |> Option.defaultValue 0
            let thread = partner |> Option.map (fun p -> loadMessages m p lastId) |> Option.defaultValue Cmd.none
            App m, Cmd.batch [ thread; loadConversations m ]
        | _ -> App m, Cmd.none
    | SetMessageDraft text -> App { m with MessageDraft = text }, Cmd.none
    | SendMessage ->
        match m.Page with
        | Chat(Some partner) when m.MessageDraft.Trim() <> "" ->
            App { m with MessageDraft = "" },
            Cmd.OfAsync.either (fun () -> m.Api.sendMessage partner.Id m.MessageDraft) () MessageSent Failed
        | _ -> App m, Cmd.none
    // The hub echoes our own messages too, but adding the message here doesn't depend on the live connection.
    // Appending skips duplicates.
    | MessageSent(Ok message) -> App(appendMessages message.RecipientId [ message ] m), loadConversations m

    | WorkflowsLoaded workflows -> App { m with Workflows = workflows }, Cmd.none
    | TasksLoaded tasks -> App { m with Tasks = tasks }, Cmd.none
    | TasksChanged -> App m, loadTasks m
    | DirectoryLoaded people -> App { m with Directory = people }, Cmd.none
    | NewWorkflow ->
        // Start in the group being viewed, or else the first group the user belongs to.
        let group =
            match m.Page with
            | Feed(GroupFeed g) -> g
            | _ -> m.Groups |> List.tryFind (fun g -> g.IsMember) |> Option.map (fun g -> g.Id) |> Option.defaultValue 0

        navigate WorkflowEditor { m with Editor = { emptyDraft with GroupId = group } } |> stay
    | OpenWorkflowEditor w -> navigate WorkflowEditor { m with Editor = Workflow.toDraft w } |> stay
    | EditWorkflow draft -> App { m with Editor = draft }, Cmd.none
    | SaveWorkflow when not m.Saving ->
        match Workflow.validate m.Editor with
        | Error e -> notice e m |> stay
        | Ok() -> App { m with Saving = true; Notice = None }, call m.WorkflowApi.saveWorkflow m.Editor WorkflowSaved
    | WorkflowSaved(Ok w) ->
        let m, cmd = navigate (WorkflowsPage AllWorkflows) { m with Saving = false }
        App { m with Notice = Some(sprintf "Saved \"%s\"" w.Name) }, cmd
    | WorkflowSaved(Error e) -> notice e { m with Saving = false } |> stay
    | DeleteWorkflow id ->
        App m, Cmd.OfAsync.either (fun () -> m.WorkflowApi.deleteWorkflow id) () (fun r -> WorkflowDeleted(id, r)) Failed
    | WorkflowDeleted(id, Ok()) -> App { m with Workflows = m.Workflows |> List.filter (fun w -> w.Id <> id) }, Cmd.none
    | SetWorkflowEnabled(id, enabled) ->
        App m, Cmd.OfAsync.either (fun () -> m.WorkflowApi.setEnabled id enabled) () WorkflowUpdated Failed
    | WorkflowUpdated(Ok w) ->
        App { m with Workflows = m.Workflows |> List.map (fun x -> if x.Id = w.Id then w else x) }, Cmd.none
    | RunWorkflow id -> App { m with Notice = None }, call m.WorkflowApi.runWorkflow id WorkflowStarted
    | WorkflowStarted(Ok run) ->
        let text =
            match run.Status with
            | WaitingForInput u when u.Id = m.Me.Id -> "Fill in the form below to continue."
            | WaitingForInput u -> sprintf "Started. Waiting for %s to fill in a form." u.DisplayName
            | WaitingForApproval u -> sprintf "Started. Waiting for %s to approve." u.DisplayName
            | Completed -> sprintf "\"%s\" ran successfully." run.WorkflowName
            | Running -> "Started."
            | Stopped reason -> "Stopped: " + reason
            | RunStatus.Failed error -> "Failed: " + error

        let goToTasks =
            match run.Status with
            | WaitingForInput u when u.Id = m.Me.Id -> true
            | _ -> false

        let m, cmd = if goToTasks then navigate (WorkflowsPage MyTasks) m else m, Cmd.batch [ loadTasks m; refreshPage m ]
        App { m with Notice = Some text }, cmd
    | ResultsLoaded(id, Ok results) when m.Page = WorkflowResultsPage id -> App { m with Results = Some results }, Cmd.none
    | SetTaskAnswer(runId, key, value) ->
        let answers = m.TaskAnswers |> Map.tryFind runId |> Option.defaultValue Map.empty
        App { m with TaskAnswers = m.TaskAnswers.Add(runId, answers.Add(key, value)) }, Cmd.none
    | SetTaskComment(runId, text) -> App { m with TaskComments = m.TaskComments.Add(runId, text) }, Cmd.none
    | SubmitTask runId ->
        let answers = m.TaskAnswers |> Map.tryFind runId |> Option.defaultValue Map.empty |> Map.toList
        App m, Cmd.OfAsync.either (fun () -> m.WorkflowApi.submitForm runId answers) () (fun r -> TaskDone(runId, r)) Failed
    | DecideTask(runId, approve) ->
        let comment = m.TaskComments |> Map.tryFind runId |> Option.defaultValue ""

        App m,
        Cmd.OfAsync.either (fun () -> m.WorkflowApi.decideApproval runId approve comment) () (fun r -> TaskDone(runId, r)) Failed
    | TaskDone(runId, Ok()) ->
        App
            { m with
                Tasks = m.Tasks |> List.filter (fun t -> t.RunId <> runId)
                TaskAnswers = m.TaskAnswers.Remove runId
                TaskComments = m.TaskComments.Remove runId
                Notice = Some "Done. The workflow has moved on." },
        Cmd.batch [ loadTasks m; refreshPage m ]

    | PostCreated(Error e)
    | PostDeleted(_, Error e)
    | CommentAdded(_, Error e)
    | GroupCreated(Error e)
    | ProfileSaved(Error e)
    | MessageSent(Error e)
    | WorkflowDeleted(_, Error e)
    | WorkflowUpdated(Error e)
    | WorkflowStarted(Error e)
    | ResultsLoaded(_, Error e)
    | TaskDone(_, Error e) -> notice e m |> stay

    | _ -> App m, Cmd.none

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match model, msg with
    | Starting, SessionRestored(token, me) -> startApp token me
    | Starting, SessionInvalid ->
        Api.TokenStore.clear ()
        Auth emptyAuth, Cmd.none
    | Auth a, _ -> updateAuth msg a
    | App m, _ -> updateApp msg m
    | Starting, _ -> model, Cmd.none

/// While signed in, keep a live connection that delivers chat messages and workflow task changes.
let subscribe (model: Model) : Sub<Msg> =
    match model with
    | App m ->
        [ [ "realtime"; string m.Me.Id ],
          fun dispatch ->
              Realtime.connect
                  m.Token
                  (MessageReceived >> dispatch)
                  (fun () -> dispatch TasksChanged)
                  (fun () ->
                      dispatch CatchUpChat
                      dispatch TasksChanged) ]
    | _ -> []
