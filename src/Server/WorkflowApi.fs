/// Implementation of IWorkflowApi for one signed-in user.
module WorkflowApi

open Shared
open Workflows
open WorkflowDb
open WorkflowEngine

/// Steps must point at people and groups that exist, and the bot only posts in groups the owner belongs to,
/// as people can only post in their own groups. Checked on save, on top of `Workflow.validate`.
let private checkReferences (db: Db.Store) (owner: User) (draft: WorkflowDraft) =
    let groupExists id = (db.GetGroup(0, id)).IsSome
    let userExists id = (db.GetUser id).IsSome

    let recipientOk =
        function
        | SpecificUser id -> userExists id
        | TriggeringUser -> true

    let triggerOk =
        match draft.Trigger with
        | NewGroupPost(groupId, _) -> groupExists groupId
        | _ -> true

    let badStep =
        draft.Steps
        |> List.tryFindIndex (function
            | SendMessage(r, _)
            | CollectInput(r, _, _)
            | RequestApproval(r, _) -> not (recipientOk r)
            | PostToGroup(g, _) -> not (groupExists g)
            | Condition _ -> false)

    let notMember =
        draft.Steps
        |> List.tryFindIndex (function
            | PostToGroup(g, _) -> groupExists g && not (db.IsMember(owner.Id, g))
            | _ -> false)

    match triggerOk, badStep, notMember with
    | false, _, _ -> Error "The trigger's group no longer exists"
    | _, Some i, _ -> Error(sprintf "Step %d: that person or group no longer exists" (i + 1))
    | _, _, Some i -> Error(sprintf "Step %d: join the group before posting in it" (i + 1))
    | true, None, None -> Ok()

let private toTask (run: RunRow) : PendingTask option =
    let render = Workflow.render run.Variables

    let kind =
        match run.Steps |> List.tryItem run.CurrentStep with
        | Some(CollectInput(_, title, fields)) when run.Status = "input" -> Some(FormTask(render title, fields))
        | Some(RequestApproval(_, message)) when run.Status = "approval" -> Some(ApprovalTask(render message))
        | _ -> None

    kind
    |> Option.map (fun kind ->
        { RunId = run.Id
          WorkflowName = run.WorkflowName
          RequestedBy = run.TriggeredBy
          Kind = kind
          CreatedAt = run.Log |> List.tryLast |> Option.map (fun e -> e.At) |> Option.defaultValue run.CreatedAt })

let private groupNameLookup (db: Db.Store) id =
    db.GetGroup(0, id) |> Option.map (fun g -> g.Name) |> Option.defaultValue "a deleted group"

let private isOwner (me: User) (w: Workflow) = w.Owner.Id = me.Id

/// Manual workflows can be started by their owner (even while off, to test) and by members of their group.
let canStart (db: Db.Store) (me: User) (w: Workflow) =
    w.Trigger = Manual && (isOwner me w || (w.Enabled && db.IsMember(me.Id, w.GroupId)))

let canSeeResults (db: Db.Store) (me: User) (w: Workflow) =
    isOwner me w || (w.ResultsVisibility = GroupMembers && db.IsMember(me.Id, w.GroupId))

let private card (db: Db.Store) (me: User) (runCount: int) (w: Workflow) : WorkflowCard =
    { WorkflowId = w.Id
      Name = w.Name
      Description = w.Description
      HowToRun = Workflow.howToRun (groupNameLookup db) w.Enabled w.Trigger w.Steps
      Instructions = w.Instructions
      Enabled = w.Enabled
      IsManual = w.Trigger = Manual
      RunCount = runCount
      CanStart = canStart db me w
      CanSeeResults = canSeeResults db me w
      CanEdit = isOwner me w }

/// Adds the workflow card to posts that are a workflow's home in its group.
let decoratePosts (db: Db.Store) (store: WorkflowStore) (me: User) (posts: Post list) : Post list =
    let workflows = store.ForPosts(posts |> List.map (fun p -> p.Id))

    if workflows.IsEmpty then
        posts
    else
        let counts = store.RunCounts(workflows |> Map.toList |> List.map (fun (_, w) -> w.Id))

        posts
        |> List.map (fun p ->
            match workflows.TryFind p.Id with
            | Some w -> { p with Workflow = Some(card db me (counts.TryFind w.Id |> Option.defaultValue 0) w) }
            | None -> p)

let create (db: Db.Store) (store: WorkflowStore) (engine: Engine) (me: User) : IWorkflowApi =
    let owned id =
        match store.Get id with
        | Some w when w.Owner.Id = me.Id -> Ok w
        | Some _ -> Error "Only the workflow's owner can do that"
        | None -> Error "That workflow no longer exists"

    { getWorkflows = fun () -> async { return store.ListFor me.Id }

      saveWorkflow =
        fun draft ->
            async {
                let ownership = if draft.Id = 0 then Ok() else owned draft.Id |> Result.map ignore

                let membership () =
                    if draft.GroupId <= 0 || db.IsMember(me.Id, draft.GroupId) then
                        Ok()
                    else
                        Error "Join the group before adding a workflow to it"

                return
                    ownership
                    |> Result.bind (fun () -> Workflow.validate draft)
                    |> Result.bind membership
                    |> Result.bind (fun () -> checkReferences db me draft)
                    |> Result.map (fun () -> store.Save(me.Id, draft) |> store.Get |> Option.get)
            }

      deleteWorkflow = fun id -> async { return owned id |> Result.map (fun w -> store.Delete w.Id) }

      setEnabled =
        fun id enabled ->
            async {
                return
                    owned id
                    |> Result.map (fun w ->
                        store.SetEnabled(w.Id, enabled)
                        store.Get w.Id |> Option.get)
            }

      runWorkflow =
        fun id ->
            async {
                match store.Get id with
                | None -> return Error "That workflow no longer exists"
                | Some w when w.Trigger <> Manual -> return Error "This workflow starts by itself and can't be started by hand"
                | Some w when not (canStart db me w) && not w.Enabled -> return Error "This workflow is turned off"
                | Some w when not (canStart db me w) -> return Error(sprintf "Join %s to start this workflow" w.GroupName)
                | Some w ->
                    let! run = engine.Start(w, Some me, Map.empty)
                    return Ok run
            }

      getResults =
        fun id ->
            async {
                return
                    match store.Get id with
                    | None -> Error "That workflow no longer exists"
                    | Some w when not (canSeeResults db me w) && w.ResultsVisibility = OwnerOnly ->
                        Error "Only the workflow's owner can see its results"
                    | Some w when not (canSeeResults db me w) -> Error(sprintf "Join %s to see this workflow's results" w.GroupName)
                    | Some w ->
                        Ok
                            { Workflow = w
                              Runs = store.RunsFor w.Id |> List.map toApiRun
                              CanEdit = isOwner me w }
            }

      getTasks = fun () -> async { return store.WaitingOn me.Id |> List.choose toTask }

      submitForm = fun runId answers -> engine.SubmitForm(me, runId, Map.ofList answers)

      decideApproval = fun runId approve comment -> engine.Decide(me, runId, approve, comment) }
