module WorkflowTests

open System
open System.IO
open Microsoft.Extensions.Logging.Abstractions
open Xunit
open Shared
open Workflows
open WorkflowEngine

// ---- Pure helpers ------------------------------------------------------------------

[<Fact>]
let ``fieldKey turns labels into variable names`` () =
    Assert.Equal("due_date", Workflow.fieldKey "Due date")
    Assert.Equal("type", Workflow.fieldKey "  Type! ")
    Assert.Equal("", Workflow.fieldKey "???")

[<Fact>]
let ``render fills known variables and blanks unknown ones`` () =
    let vars = Map [ "form.a", "x" ]
    Assert.Equal("x & ", Workflow.render vars "{{form.a}} & {{ missing }}")

let private draft trigger steps =
    { Id = 0; Name = "Test"; Description = ""; Trigger = trigger; Steps = steps }

let private form label = CollectInput(TriggeringUser, "Form", [ { Label = label; Kind = ShortText; Required = true } ])

[<Fact>]
let ``validate accepts form answers used after the form`` () =
    let result = Workflow.validate (draft Manual [ form "Reason"; SendMessage(TriggeringUser, "You said {{form.reason}}") ])
    Assert.Equal(Ok(), result)

[<Fact>]
let ``validate rejects variables used before they exist`` () =
    match Workflow.validate (draft Manual [ SendMessage(TriggeringUser, "{{form.reason}}"); form "Reason" ]) with
    | Error e -> Assert.Contains("{{form.reason}}", e)
    | Ok() -> failwith "expected an error"

[<Fact>]
let ``validate only offers post variables to group triggers`` () =
    let steps = [ SendMessage(TriggeringUser, "{{trigger.post}}") ]
    Assert.True(Result.isError (Workflow.validate (draft Manual steps)))
    Assert.Equal(Ok(), Workflow.validate (draft (NewGroupPost(1, "")) steps))

[<Fact>]
let ``validate rejects fields that would share a variable`` () =
    Assert.True(Result.isError (Workflow.validate (draft Manual [ form "Due date"; form "Due-date" ])))

[<Fact>]
let ``validate requires steps, a name and schedule days`` () =
    Assert.True(Result.isError (Workflow.validate (draft Manual [])))
    Assert.True(Result.isError (Workflow.validate { draft Manual [ form "A" ] with Name = " " }))
    let noDays = Scheduled { Days = []; Hour = 9; Minute = 0; TimeZone = "UTC" }
    Assert.True(Result.isError (Workflow.validate (draft noDays [ form "A" ])))

// ---- Schedules ---------------------------------------------------------------------

let private utc (y, mo, d, h, mi) = DateTime(y, mo, d, h, mi, 0, DateTimeKind.Utc)
let private mondayNine = { Days = [ 1 ]; Hour = 9; Minute = 0; TimeZone = "UTC" }

[<Fact>]
let ``latest occurrence is today once the time has passed`` () =
    // 2026-10-26 is a Monday.
    Assert.Equal(Some(utc (2026, 10, 26, 9, 0)), Schedule.latestOccurrence mondayNine (utc (2026, 10, 26, 9, 5)))

[<Fact>]
let ``latest occurrence is last week before the time`` () =
    Assert.Equal(Some(utc (2026, 10, 19, 9, 0)), Schedule.latestOccurrence mondayNine (utc (2026, 10, 26, 8, 59)))

[<Fact>]
let ``schedules use their own time zone`` () =
    // New York is on daylight time (UTC-4) until 1 November 2026.
    let ny = { mondayNine with TimeZone = "America/New_York" }
    Assert.Equal(Some(utc (2026, 10, 26, 13, 0)), Schedule.latestOccurrence ny (utc (2026, 10, 26, 13, 30)))

[<Fact>]
let ``due fires once, and not after the grace period`` () =
    let lastWeek = utc (2026, 10, 19, 9, 0)
    Assert.Equal(Some(utc (2026, 10, 26, 9, 0)), Schedule.due mondayNine lastWeek (utc (2026, 10, 26, 9, 1)))
    Assert.Equal(None, Schedule.due mondayNine (utc (2026, 10, 26, 9, 0)) (utc (2026, 10, 26, 9, 2)))
    Assert.Equal(None, Schedule.due mondayNine lastWeek (utc (2026, 10, 26, 10, 0)))

// ---- Engine ------------------------------------------------------------------------

type private Env =
    { Db: Db.Store
      Store: WorkflowDb.WorkflowStore
      Engine: Engine
      Bot: User
      Messages: ResizeArray<ChatMessage>
      TaskPings: ResizeArray<int>
      User: string -> User }

let private newEnv () =
    let path = Path.Combine(Path.GetTempPath(), $"workflow-tests-{Guid.NewGuid()}.db")
    let db = Db.Store($"Data Source={path};Pooling=False")
    db.Initialize()
    let store = WorkflowDb.WorkflowStore(db)
    store.Initialize()
    let bot = store.EnsureBot()

    let users =
        [ "requester"; "approver"; "other" ]
        |> List.map (fun name ->
            let req = { Username = name; Password = "password123"; DisplayName = name; Title = ""; Department = "" }
            name, (db.CreateUser(req, "x") |> Result.toOption |> Option.get))
        |> Map.ofList

    let messages = ResizeArray()
    let pings = ResizeArray()

    let engine =
        Engine(
            db,
            store,
            bot,
            (fun m -> async { messages.Add m }),
            (fun id -> async { pings.Add id }),
            NullLogger.Instance
        )

    { Db = db
      Store = store
      Engine = engine
      Bot = bot
      Messages = messages
      TaskPings = pings
      User = fun name -> users[name] }

let private save (env: Env) owner trigger steps =
    let id = env.Store.Save((env.User owner).Id, draft trigger steps)
    env.Store.Get id |> Option.get

let private timeOff (env: Env) =
    [ CollectInput(
          TriggeringUser,
          "Time off",
          [ { Label = "Dates"; Kind = ShortText; Required = true }
            { Label = "Type"; Kind = Dropdown "Vacation, Sick"; Required = true } ]
      )
      RequestApproval(SpecificUser (env.User "approver").Id, "{{trigger.user}} wants {{form.type}}")
      SendMessage(TriggeringUser, "Approved by {{approval.by}} for {{form.dates}}") ]

let private run (a: Async<'T>) = Async.RunSynchronously a

let private statusOf (env: Env) runId =
    (env.Store.GetRun runId |> Option.get |> toApiRun).Status

[<Fact>]
let ``a run pauses for the form, then the approval, then finishes`` () =
    let env = newEnv ()
    let requester, approver = env.User "requester", env.User "approver"
    let workflow = save env "approver" Manual (timeOff env)

    let started = run (env.Engine.Start(workflow, Some requester, Map.empty))
    Assert.Equal(WaitingForInput requester, started.Status)
    Assert.Contains(requester.Id, env.TaskPings)

    let answers = Map [ "dates", "1-3 May"; "type", "Vacation" ]
    Assert.Equal(Error "\"Type\" is required", run (env.Engine.SubmitForm(requester, started.Id, answers.Remove "type")))
    Assert.True(Result.isError (run (env.Engine.SubmitForm(requester, started.Id, answers.Add("type", "Party")))))
    Assert.Equal(Error "This task is assigned to someone else", run (env.Engine.SubmitForm(approver, started.Id, answers)))

    Assert.Equal(Ok(), run (env.Engine.SubmitForm(requester, started.Id, answers)))
    Assert.Equal(WaitingForApproval approver, statusOf env started.Id)

    Assert.Equal(Ok(), run (env.Engine.Decide(approver, started.Id, true, "Have fun")))
    Assert.Equal(Completed, statusOf env started.Id)

    let message = Assert.Single env.Messages
    Assert.Equal(env.Bot.Id, message.SenderId)
    Assert.Equal(requester.Id, message.RecipientId)
    Assert.Contains("Approved by approver for 1-3 May", message.Body)

    // The task is gone once it's done.
    Assert.True(Result.isError (run (env.Engine.Decide(approver, started.Id, true, ""))))

[<Fact>]
let ``a rejection stops the run and tells the requester`` () =
    let env = newEnv ()
    let requester, approver = env.User "requester", env.User "approver"
    let workflow = save env "approver" Manual (timeOff env)
    let started = run (env.Engine.Start(workflow, Some requester, Map.empty))
    run (env.Engine.SubmitForm(requester, started.Id, Map [ "dates", "Friday"; "type", "Sick" ])) |> ignore

    Assert.Equal(Ok(), run (env.Engine.Decide(approver, started.Id, false, "Busy week")))

    Assert.Equal(Stopped "Rejected by approver: Busy week", statusOf env started.Id)
    Assert.Contains("rejected by approver: Busy week", (Assert.Single env.Messages).Body)

[<Fact>]
let ``a failing condition stops the run`` () =
    let env = newEnv ()
    let steps =
        [ CollectInput(TriggeringUser, "Kind", [ { Label = "Type"; Kind = ShortText; Required = true } ])
          Condition("{{form.type}}", Equals, "vacation")
          SendMessage(TriggeringUser, "Only for vacations") ]

    let workflow = save env "approver" Manual steps
    let requester = env.User "requester"

    let first = run (env.Engine.Start(workflow, Some requester, Map.empty))
    run (env.Engine.SubmitForm(requester, first.Id, Map [ "type", "Sick" ])) |> ignore
    Assert.Equal(Stopped "Condition not met (\"Sick\" equals \"vacation\")", statusOf env first.Id)
    Assert.Empty env.Messages

    // Matching is case-insensitive.
    let second = run (env.Engine.Start(workflow, Some requester, Map.empty))
    run (env.Engine.SubmitForm(requester, second.Id, Map [ "type", "Vacation" ])) |> ignore
    Assert.Equal(Completed, statusOf env second.Id)

[<Fact>]
let ``a paused run keeps the steps it started with`` () =
    let env = newEnv ()
    let requester = env.User "requester"
    let workflow = save env "approver" Manual (timeOff env)
    let started = run (env.Engine.Start(workflow, Some requester, Map.empty))

    env.Store.Save((env.User "approver").Id, { Workflow.toDraft workflow with Steps = [ SendMessage(TriggeringUser, "Changed") ] })
    |> ignore

    run (env.Engine.SubmitForm(requester, started.Id, Map [ "dates", "Monday"; "type", "Sick" ])) |> ignore
    Assert.Equal(WaitingForApproval(env.User "approver"), statusOf env started.Id)

[<Fact>]
let ``group posts start matching workflows, but bot posts never do`` () =
    let env = newEnv ()
    let poster = env.User "requester"
    let groupId = env.Db.CreateGroup(poster.Id, { Name = "Eng"; Description = "" }) |> Result.toOption |> Option.get
    let watcher = env.User "approver"
    save env "approver" (NewGroupPost(groupId, "incident")) [ SendMessage(SpecificUser watcher.Id, "{{trigger.user}}: {{trigger.post}}") ]
    |> ignore

    let post author body =
        let id = env.Db.CreatePost(author, Some groupId, body)
        env.Db.GetPost(poster.Id, id) |> Option.get

    run (env.Engine.OnPostCreated(post poster.Id "All quiet today"))
    Assert.Empty env.Messages

    run (env.Engine.OnPostCreated(post poster.Id "Small INCIDENT in prod"))
    Assert.Contains("requester: Small INCIDENT in prod", (Assert.Single env.Messages).Body)

    run (env.Engine.OnPostCreated(post env.Bot.Id "Bot mentions an incident"))
    Assert.Single env.Messages |> ignore

[<Fact>]
let ``scheduled workflows fire once per occurrence`` () =
    let env = newEnv ()
    let groupId = env.Db.CreateGroup((env.User "other").Id, { Name = "All"; Description = "" }) |> Result.toOption |> Option.get
    // Saving resets the schedule clock to now, so schedule a time a few minutes ahead and check from there.
    let at = DateTime.UtcNow.AddMinutes 5.0
    let schedule = { Days = [ 0..6 ]; Hour = at.Hour; Minute = at.Minute; TimeZone = "UTC" }
    let workflow = save env "other" (Scheduled schedule) [ PostToGroup(groupId, "Stand-up time") ]
    let later = at.AddMinutes 2.0

    run (env.Engine.RunDueSchedules later)
    run (env.Engine.RunDueSchedules later)

    let runs = env.Store.RunsFor workflow.Id
    Assert.Equal(Completed, (toApiRun (Assert.Single runs)).Status)
    Assert.Equal("Stand-up time", (env.Db.GetFeed((env.User "other").Id, GroupFeed groupId) |> List.head).Body)

// ---- API permissions -----------------------------------------------------------

[<Fact>]
let ``only owners edit workflows, and others run only enabled manual ones`` () =
    let env = newEnv ()
    let ownerApi = WorkflowApi.create env.Db env.Store env.Engine (env.User "approver")
    let otherApi = WorkflowApi.create env.Db env.Store env.Engine (env.User "other")
    let steps = [ SendMessage(TriggeringUser, "Hi {{trigger.user}}") ]

    let created =
        match run (ownerApi.saveWorkflow (draft Manual steps)) with
        | Ok w -> w
        | Error e -> failwith e

    Assert.True(Result.isError (run (otherApi.saveWorkflow { Workflow.toDraft created with Name = "Mine now" })))
    Assert.True(Result.isError (run (otherApi.deleteWorkflow created.Id)))
    Assert.True(Result.isError (run (otherApi.getRuns created.Id)))

    Assert.True(Result.isOk (run (otherApi.runWorkflow created.Id)))
    run (ownerApi.setEnabled created.Id false) |> ignore
    Assert.True(Result.isError (run (otherApi.runWorkflow created.Id)))
    Assert.True(Result.isOk (run (ownerApi.runWorkflow created.Id)))

    // Saving refuses people that don't exist.
    let ghost = draft Manual [ SendMessage(SpecificUser 9999, "Boo") ]
    Assert.True(Result.isError (run (ownerApi.saveWorkflow ghost)))

    // The bot only posts in groups the owner belongs to.
    let groupId = env.Db.CreateGroup((env.User "other").Id, { Name = "Private-ish"; Description = "" }) |> Result.toOption |> Option.get
    let post = draft Manual [ PostToGroup(groupId, "Hello") ]
    Assert.Equal(Error "Step 1: join the group before posting in it", run (ownerApi.saveWorkflow post) |> Result.map ignore)
    Assert.True(Result.isOk (run (otherApi.saveWorkflow post)))

[<Fact>]
let ``the demo workflows are valid`` () =
    let path = Path.Combine(Path.GetTempPath(), $"workflow-seed-{Guid.NewGuid()}.db")
    let db = Db.Store($"Data Source={path};Pooling=False")
    db.Initialize()
    let store = WorkflowDb.WorkflowStore(db)
    store.Initialize()
    Seed.run db store

    let workflows = (store.ListFor 0) @ (store.Scheduled() |> List.map fst) @ (store.ForGroupPosts 2)
    Assert.Equal(3, workflows |> List.distinctBy (fun w -> w.Id) |> List.length)

    for w in workflows do
        Assert.Equal(Ok(), Workflow.validate (Workflow.toDraft w))
