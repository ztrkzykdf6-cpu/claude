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

                return
                    ownership
                    |> Result.bind (fun () -> Workflow.validate draft)
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
                | Some w when w.Trigger <> Manual -> return Error "This workflow starts automatically and can't be run by hand"
                // The owner may test a workflow before turning it on.
                | Some w when not w.Enabled && w.Owner.Id <> me.Id -> return Error "This workflow is turned off"
                | Some w ->
                    let! run = engine.Start(w, Some me, Map.empty)
                    return Ok run
            }

      getRuns =
        fun id -> async { return owned id |> Result.map (fun w -> store.RunsFor w.Id |> List.map toApiRun) }

      getTasks = fun () -> async { return store.WaitingOn me.Id |> List.choose toTask }

      submitForm = fun runId answers -> engine.SubmitForm(me, runId, Map.ofList answers)

      decideApproval = fun runId approve comment -> engine.Decide(me, runId, approve, comment) }
