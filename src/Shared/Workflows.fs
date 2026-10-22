/// Workflow Builder: types, validation and the API contract, shared by the server and the client.
module Workflows

open System
open System.Text.RegularExpressions
open Shared

type FieldKind =
    | ShortText
    | LongText
    /// Choices as a comma-separated list, kept as typed so the editor doesn't fight the cursor.
    | Dropdown of options: string

type FormField =
    { Label: string
      Kind: FieldKind
      Required: bool }

/// Days use 0 = Sunday … 6 = Saturday; the time is in the IANA `TimeZone` of whoever set it up.
type Schedule =
    { Days: int list
      Hour: int
      Minute: int
      TimeZone: string }

type Trigger =
    /// Started from the Run button by anyone.
    | Manual
    /// A post in the group; an empty keyword matches every post.
    | NewGroupPost of groupId: int * keyword: string
    | Scheduled of Schedule

type Recipient =
    /// Whoever started the run (the poster for group triggers, the owner for scheduled runs).
    | TriggeringUser
    | SpecificUser of userId: int

type CompareOp =
    | Equals
    | NotEquals
    | Contains

type Step =
    | SendMessage of recipient: Recipient * text: string
    | PostToGroup of groupId: int * text: string
    /// Pauses the run until the assignee submits the form; answers become `{{form.<key>}}`.
    | CollectInput of assignee: Recipient * title: string * fields: FormField list
    /// Pauses the run until the approver decides; a rejection stops the run.
    | RequestApproval of approver: Recipient * message: string
    /// Continues only when the comparison holds (case-insensitive); otherwise the run stops.
    | Condition of left: string * op: CompareOp * right: string

type Workflow =
    { Id: int
      Owner: User
      Name: string
      Description: string
      Enabled: bool
      Trigger: Trigger
      Steps: Step list
      UpdatedAt: DateTime }

/// What the builder saves; `Id = 0` creates a new workflow.
type WorkflowDraft =
    { Id: int
      Name: string
      Description: string
      Trigger: Trigger
      Steps: Step list }

type RunStatus =
    | Running
    | WaitingForInput of User
    | WaitingForApproval of User
    | Completed
    | Stopped of reason: string
    | Failed of error: string

type RunLogEntry = { At: DateTime; Text: string }

type WorkflowRun =
    { Id: int
      WorkflowId: int
      WorkflowName: string
      TriggeredBy: User option
      Status: RunStatus
      StartedAt: DateTime
      Log: RunLogEntry list }

type TaskKind =
    | FormTask of title: string * fields: FormField list
    | ApprovalTask of message: string

/// A paused run waiting on the current user.
type PendingTask =
    { RunId: int
      WorkflowName: string
      RequestedBy: User option
      Kind: TaskKind
      CreatedAt: DateTime }

module Workflow =
    let maxSteps = 20

    let dayNames = [| "Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat" |]

    /// The variable name a form field's answer is stored under: "Due date" → "due_date".
    let fieldKey (label: string) =
        let chars =
            label.Trim().ToLowerInvariant()
            |> Seq.map (fun c -> if Char.IsLetterOrDigit c then c else '_')
            |> Seq.toArray

        Regex.Replace(String(chars), "_+", "_").Trim('_')

    let options (csv: string) =
        csv.Split(',') |> Array.map (fun o -> o.Trim()) |> Array.filter (fun o -> o <> "") |> Array.toList

    let private templateVariable = Regex(@"\{\{\s*([\w.]+)\s*\}\}")

    /// Variable names referenced as `{{name}}` in a template.
    let referencedVariables (template: string) =
        [ for m in templateVariable.Matches template -> m.Groups[1].Value ]

    /// Replaces `{{name}}` with its value; unknown names render as empty text.
    let render (variables: Map<string, string>) (template: string) =
        templateVariable.Replace(
            template,
            fun (m: Match) -> variables |> Map.tryFind m.Groups[1].Value |> Option.defaultValue ""
        )

    /// Variables a step at `index` can use, with a short description of each.
    let variablesBefore (trigger: Trigger) (steps: Step list) (index: int) =
        [ "workflow.name", "This workflow's name"
          "trigger.user", "Who started the run"
          match trigger with
          | NewGroupPost _ ->
              "trigger.post", "Text of the post"
              "trigger.group", "Name of the group"
          | _ -> ()
          for step in List.truncate index steps do
              match step with
              | CollectInput(_, _, fields) ->
                  for f in fields do
                      "form." + fieldKey f.Label, sprintf "Answer to \"%s\"" f.Label
              | RequestApproval _ ->
                  "approval.by", "Who approved"
                  "approval.comment", "The approver's comment"
              | _ -> () ]
        |> List.distinctBy fst

    let private stepTemplates step =
        match step with
        | SendMessage(_, text)
        | PostToGroup(_, text)
        | RequestApproval(_, text) -> [ text ]
        | CollectInput(_, title, _) -> [ title ]
        | Condition(left, _, right) -> [ left; right ]

    let private stepErrors (step: Step) =
        let recipientOk =
            function
            | TriggeringUser -> true
            | SpecificUser id -> id > 0

        [ match step with
          | SendMessage(recipient, text) ->
              if not (recipientOk recipient) then "Choose who receives the message"
              if String.IsNullOrWhiteSpace text then "Write the message to send"
          | PostToGroup(groupId, text) ->
              if groupId <= 0 then "Choose a group to post in"
              if String.IsNullOrWhiteSpace text then "Write the post"
          | CollectInput(assignee, title, fields) ->
              if not (recipientOk assignee) then "Choose who fills in the form"
              if String.IsNullOrWhiteSpace title then "Give the form a title"
              if fields.IsEmpty then "Add at least one form field"
              for f in fields do
                  if fieldKey f.Label = "" then "Every form field needs a label"
                  match f.Kind with
                  | Dropdown csv when (options csv).IsEmpty -> sprintf "Add choices to the \"%s\" dropdown" f.Label
                  | _ -> ()
          | RequestApproval(approver, message) ->
              if not (recipientOk approver) then "Choose an approver"
              if String.IsNullOrWhiteSpace message then "Write what is being approved"
          | Condition(left, _, _) -> if String.IsNullOrWhiteSpace left then "Choose what the condition checks" ]

    /// Checks a draft before saving; the server runs the same checks.
    let validate (draft: WorkflowDraft) : Result<unit, string> =
        let errors =
            [ if String.IsNullOrWhiteSpace draft.Name then "Give the workflow a name"
              if draft.Name.Length > 80 then "Keep the name under 80 characters"
              match draft.Trigger with
              | Manual -> ()
              | NewGroupPost(groupId, _) -> if groupId <= 0 then "Choose the group that triggers the workflow"
              | Scheduled s ->
                  if s.Days.IsEmpty then "Pick at least one day for the schedule"
                  if s.Hour < 0 || s.Hour > 23 || s.Minute < 0 || s.Minute > 59 then "Pick a valid time"
                  if String.IsNullOrWhiteSpace s.TimeZone then "The schedule needs a time zone"
              if draft.Steps.IsEmpty then "Add at least one step"
              if draft.Steps.Length > maxSteps then sprintf "Workflows are limited to %d steps" maxSteps

              let keys =
                  [ for step in draft.Steps do
                        match step with
                        | CollectInput(_, _, fields) -> yield! fields |> List.map (fun f -> fieldKey f.Label)
                        | _ -> () ]
                  |> List.filter ((<>) "")

              for key in keys |> List.countBy id |> List.filter (fun (_, n) -> n > 1) |> List.map fst do
                  sprintf "Two form fields would both be stored as {{form.%s}}; rename one" key

              for i, step in List.indexed draft.Steps do
                  for e in stepErrors step do
                      sprintf "Step %d: %s" (i + 1) e

                  let known = variablesBefore draft.Trigger draft.Steps i |> List.map fst |> Set.ofList

                  for v in stepTemplates step |> List.collect referencedVariables |> List.distinct do
                      if not (known.Contains v) then
                          sprintf "Step %d: {{%s}} isn't available at this point" (i + 1) v ]

        match errors with
        | [] -> Ok()
        | first :: _ -> Error first

    let toDraft (w: Workflow) : WorkflowDraft =
        { Id = w.Id
          Name = w.Name
          Description = w.Description
          Trigger = w.Trigger
          Steps = w.Steps }

/// Workflow endpoints; like IWorkplaceApi they need a bearer token.
type IWorkflowApi =
    { /// Workflows the user owns, plus everyone's enabled manual workflows.
      getWorkflows: unit -> Async<Workflow list>
      saveWorkflow: WorkflowDraft -> Async<Result<Workflow, string>>
      deleteWorkflow: int -> Async<Result<unit, string>>
      setEnabled: int -> bool -> Async<Result<Workflow, string>>
      runWorkflow: int -> Async<Result<WorkflowRun, string>>
      /// Recent runs of a workflow the user owns.
      getRuns: int -> Async<Result<WorkflowRun list, string>>
      getTasks: unit -> Async<PendingTask list>
      submitForm: int -> (string * string) list -> Async<Result<unit, string>>
      decideApproval: int -> bool -> string -> Async<Result<unit, string>> }
