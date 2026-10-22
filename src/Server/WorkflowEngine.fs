/// Runs workflows: executes steps in order, pauses for forms and approvals, and fires triggers.
module WorkflowEngine

open System
open System.Threading
open System.Threading.Tasks
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging
open Shared
open Workflows
open WorkflowDb

module Schedule =
    /// How long after its time a scheduled run may still fire, e.g. after a restart.
    let gracePeriod = TimeSpan.FromMinutes 15.0

    let private timeZone (id: string) =
        match TimeZoneInfo.TryFindSystemTimeZoneById id with
        | true, tz -> tz
        | _ -> TimeZoneInfo.Utc

    /// The most recent scheduled time at or before `nowUtc`, in UTC.
    let latestOccurrence (schedule: Schedule) (nowUtc: DateTime) : DateTime option =
        let tz = timeZone schedule.TimeZone
        let localNow = TimeZoneInfo.ConvertTimeFromUtc(nowUtc, tz)

        seq { 0..7 }
        |> Seq.map (fun daysBack -> localNow.Date.AddDays(float -daysBack).AddHours(float schedule.Hour).AddMinutes(float schedule.Minute))
        |> Seq.filter (fun t -> t <= localNow && List.contains (int t.DayOfWeek) schedule.Days)
        // A time skipped by a daylight-saving jump has no UTC equivalent; that day is skipped.
        |> Seq.filter (fun t -> not (tz.IsInvalidTime t))
        |> Seq.tryHead
        |> Option.map (fun t -> TimeZoneInfo.ConvertTimeToUtc(DateTime.SpecifyKind(t, DateTimeKind.Unspecified), tz))

    /// The occurrence to fire now, if one passed since `lastFired` and is still within the grace period.
    let due (schedule: Schedule) (lastFired: DateTime) (nowUtc: DateTime) =
        latestOccurrence schedule nowUtc
        |> Option.filter (fun t -> t > lastFired.ToUniversalTime() && nowUtc - t <= gracePeriod)

let private describeOp =
    function
    | Equals -> "equals"
    | NotEquals -> "does not equal"
    | Contains -> "contains"

let private compare op (left: string) (right: string) =
    let l, r = left.Trim(), right.Trim()

    match op with
    | Equals -> String.Equals(l, r, StringComparison.OrdinalIgnoreCase)
    | NotEquals -> not (String.Equals(l, r, StringComparison.OrdinalIgnoreCase))
    | Contains -> l.Contains(r, StringComparison.OrdinalIgnoreCase)

let toApiRun (row: RunRow) : WorkflowRun =
    let waiting make =
        match row.WaitingOn with
        | Some u -> make u
        | None -> Failed "The person this step was waiting on no longer exists"

    { Id = row.Id
      WorkflowId = row.WorkflowId
      WorkflowName = row.WorkflowName
      TriggeredBy = row.TriggeredBy
      StartedAt = row.CreatedAt
      Log = row.Log
      Status =
        match row.Status with
        | "running" -> Running
        | "input" -> waiting WaitingForInput
        | "approval" -> waiting WaitingForApproval
        | "completed" -> Completed
        | "stopped" -> Stopped row.Detail
        | _ -> Failed row.Detail }

type Engine
    (
        db: Db.Store,
        store: WorkflowStore,
        bot: User,
        notifyMessage: ChatMessage -> Async<unit>,
        notifyTasks: int -> Async<unit>,
        logger: ILogger
    ) =

    // One run changes at a time, so a double-clicked submit can't advance a run twice.
    let gate = new SemaphoreSlim(1, 1)

    let locked (work: unit -> Async<'T>) =
        async {
            do! gate.WaitAsync() |> Async.AwaitTask

            try
                return! work ()
            finally
                gate.Release() |> ignore
        }

    let log text (run: RunRow) =
        { run with Log = run.Log @ [ { At = DateTime.UtcNow; Text = text } ] }

    let finish status detail text (run: RunRow) =
        { (log text run) with Status = status; Detail = detail; WaitingOn = None }

    let resolve (run: RunRow) recipient =
        match recipient with
        | TriggeringUser ->
            match run.TriggeredBy with
            | Some u -> Some u
            | None -> db.GetUser run.OwnerId
        | SpecificUser id -> db.GetUser id

    let botMessage (recipient: User) (text: string) =
        async {
            if recipient.Id <> bot.Id then
                let message = db.SendMessage(bot.Id, recipient.Id, text)
                do! notifyMessage message
        }

    /// Executes steps from the run's current position until it finishes or pauses.
    let rec advance (run: RunRow) : Async<RunRow> =
        async {
            if run.CurrentStep >= run.Steps.Length then
                return finish "completed" "" "Workflow finished" run
            else
                let render = Workflow.render run.Variables
                let next r = { r with CurrentStep = r.CurrentStep + 1 }
                let missing = finish "failed" "A person in this workflow no longer exists" "Stopped: a person in this workflow no longer exists"

                match run.Steps[run.CurrentStep] with
                | SendMessage(recipient, text) ->
                    match resolve run recipient with
                    | Some user ->
                        do! botMessage user (sprintf "⚡ %s\n%s" run.WorkflowName (render text))
                        return! advance (run |> log (sprintf "Sent a message to %s" user.DisplayName) |> next)
                    | None -> return missing run
                | PostToGroup(groupId, text) ->
                    match db.GetGroup(bot.Id, groupId) with
                    | Some group ->
                        db.CreatePost(bot.Id, Some groupId, render text) |> ignore
                        return! advance (run |> log (sprintf "Posted in %s" group.Name) |> next)
                    | None -> return finish "failed" "The group no longer exists" "Stopped: the group no longer exists" run
                | CollectInput(assignee, title, _) ->
                    match resolve run assignee with
                    | Some user ->
                        do! notifyTasks user.Id

                        return
                            { (log (sprintf "Waiting for %s to fill in \"%s\"" user.DisplayName (render title)) run) with
                                Status = "input"
                                WaitingOn = Some user }
                    | None -> return missing run
                | RequestApproval(approver, _) ->
                    match resolve run approver with
                    | Some user ->
                        do! notifyTasks user.Id

                        return
                            { (log (sprintf "Waiting for %s to approve" user.DisplayName) run) with
                                Status = "approval"
                                WaitingOn = Some user }
                    | None -> return missing run
                | Condition(left, op, right) ->
                    let l, r = render left, render right

                    if compare op l r then
                        return! advance (run |> log "Condition met, continuing" |> next)
                    else
                        let reason = sprintf "Condition not met (\"%s\" %s \"%s\")" l (describeOp op) r
                        return finish "stopped" reason ("Stopped: " + reason) run
        }

    /// Advances a run and saves the result; a failing step marks the run failed instead of throwing.
    let advanceAndSave (run: RunRow) =
        async {
            let! result =
                async {
                    try
                        return! advance { run with Status = "running"; WaitingOn = None }
                    with ex ->
                        logger.LogError(ex, "Workflow run {RunId} failed", run.Id)
                        return finish "failed" "Something went wrong running this step" "Stopped: something went wrong" run
                }

            store.SaveRun result
            return result
        }

    /// Loads a paused run for the user it waits on, checking it is in the expected state.
    let claim (me: User) (runId: int) (status: string) =
        match store.GetRun runId with
        | Some run when run.Status = status && run.WaitingOn |> Option.exists (fun u -> u.Id = me.Id) -> Ok run
        | Some run when run.Status = status -> Error "This task is assigned to someone else"
        | Some _ -> Error "This task has already been completed"
        | None -> Error "This workflow run no longer exists"

    member _.Bot = bot

    member _.Start(workflow: Workflow, triggeredBy: User option, extra: Map<string, string>) : Async<WorkflowRun> =
        locked (fun () ->
            async {
                let starter = triggeredBy |> Option.defaultValue workflow.Owner

                let variables =
                    extra
                    |> Map.add "workflow.name" workflow.Name
                    |> Map.add "trigger.user" starter.DisplayName

                let runId = store.CreateRun(workflow, triggeredBy, variables)
                let run = store.GetRun runId |> Option.get

                let started =
                    match triggeredBy with
                    | Some u -> sprintf "Started by %s" u.DisplayName
                    | None -> "Started on schedule"

                let! result = advanceAndSave (log started run)
                return toApiRun result
            })

    member _.SubmitForm(me: User, runId: int, answers: Map<string, string>) : Async<Result<unit, string>> =
        locked (fun () ->
            async {
                match claim me runId "input" with
                | Error e -> return Error e
                | Ok run ->
                    match run.Steps[run.CurrentStep] with
                    | CollectInput(_, title, fields) ->
                        let answer (f: FormField) =
                            answers |> Map.tryFind (Workflow.fieldKey f.Label) |> Option.defaultValue "" |> fun a -> a.Trim()

                        let problems =
                            [ for f in fields do
                                  let value = answer f

                                  if f.Required && value = "" then
                                      sprintf "\"%s\" is required" f.Label
                                  else
                                      match f.Kind with
                                      | Dropdown csv when value <> "" && not (List.contains value (Workflow.options csv)) ->
                                          sprintf "Pick one of the choices for \"%s\"" f.Label
                                      | _ -> () ]

                        match problems with
                        | first :: _ -> return Error first
                        | [] ->
                            let variables =
                                fields
                                |> List.fold (fun vars f -> vars |> Map.add ("form." + Workflow.fieldKey f.Label) (answer f)) run.Variables

                            let run =
                                { run with Variables = variables; CurrentStep = run.CurrentStep + 1 }
                                |> log (sprintf "%s submitted \"%s\"" me.DisplayName (Workflow.render run.Variables title))

                            let! _ = advanceAndSave run
                            do! notifyTasks me.Id
                            return Ok()
                    | _ -> return Error "This task has already been completed"
            })

    member _.Decide(me: User, runId: int, approve: bool, comment: string) : Async<Result<unit, string>> =
        locked (fun () ->
            async {
                match claim me runId "approval" with
                | Error e -> return Error e
                | Ok run ->
                    let comment = comment.Trim()
                    let suffix = if comment = "" then "" else ": " + comment

                    if approve then
                        let run =
                            { run with
                                Variables = run.Variables |> Map.add "approval.by" me.DisplayName |> Map.add "approval.comment" comment
                                CurrentStep = run.CurrentStep + 1 }
                            |> log (sprintf "Approved by %s%s" me.DisplayName suffix)

                        let! _ = advanceAndSave run
                        ()
                    else
                        let reason = sprintf "Rejected by %s%s" me.DisplayName suffix
                        store.SaveRun(finish "stopped" reason reason run)

                        match run.TriggeredBy with
                        | Some requester when requester.Id <> me.Id ->
                            do! botMessage requester (sprintf "⚡ %s\nYour request was rejected by %s%s" run.WorkflowName me.DisplayName suffix)
                        | _ -> ()

                    do! notifyTasks me.Id
                    return Ok()
            })

    /// Starts every enabled workflow whose group-post trigger matches the post.
    member this.OnPostCreated(post: Post) : Async<unit> =
        async {
            // Posts by the bot never trigger workflows, so two workflows can't feed each other forever.
            match post.GroupId with
            | Some groupId when post.Author.Id <> bot.Id ->
                for workflow in store.ForGroupPosts groupId do
                    match workflow.Trigger with
                    | NewGroupPost(_, keyword) when
                        keyword.Trim() = "" || post.Body.Contains(keyword.Trim(), StringComparison.OrdinalIgnoreCase)
                        ->
                        let extra =
                            Map [ "trigger.post", post.Body; "trigger.group", post.GroupName |> Option.defaultValue "" ]

                        let! _ = this.Start(workflow, Some post.Author, extra)
                        ()
                    | _ -> ()
            | _ -> ()
        }

    /// Starts scheduled workflows whose time has come.
    member this.RunDueSchedules(nowUtc: DateTime) : Async<unit> =
        async {
            for workflow, lastFired in store.Scheduled() do
                match workflow.Trigger with
                | Scheduled schedule ->
                    match Schedule.due schedule lastFired nowUtc with
                    | Some occurrence ->
                        store.MarkFired(workflow.Id, occurrence)
                        let! _ = this.Start(workflow, None, Map.empty)
                        ()
                    | None -> ()
                | _ -> ()
        }

/// Checks scheduled workflows every 20 seconds.
type Scheduler(engine: Engine, logger: ILogger<Scheduler>) =
    inherit BackgroundService()

    override _.ExecuteAsync(stopping: CancellationToken) =
        task {
            while not stopping.IsCancellationRequested do
                try
                    do! engine.RunDueSchedules DateTime.UtcNow
                with ex ->
                    logger.LogError(ex, "Checking scheduled workflows failed")

                try
                    do! Task.Delay(TimeSpan.FromSeconds 20.0, stopping)
                with :? OperationCanceledException ->
                    ()
        }
