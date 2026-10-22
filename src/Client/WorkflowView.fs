/// Workflow Builder pages: workflow cards in group feeds, the workflow list, the editor, the tasks inbox and results.
module WorkflowView

open System
open Fable.Core
open Feliz
open Shared
open State
open Workflows
open Components

[<Emit("Intl.DateTimeFormat().resolvedOptions().timeZone || 'UTC'")>]
let private browserTimeZone () : string = jsNative

let private pad (n: int) = (string n).PadLeft(2, '0')

let private groupName (m: AppModel) id =
    m.Groups |> List.tryFind (fun g -> g.Id = id) |> Option.map (fun g -> g.Name) |> Option.defaultValue "a group"

let private personName (m: AppModel) recipient =
    match recipient with
    | TriggeringUser -> "the person who started it"
    | SpecificUser id ->
        m.Directory @ [ m.Me ]
        |> List.tryFind (fun u -> u.Id = id)
        |> Option.map (fun u -> u.DisplayName)
        |> Option.defaultValue "a specific person"

let describeTrigger (m: AppModel) trigger = Workflow.describeTrigger (groupName m) trigger

let private isMember (m: AppModel) groupId =
    m.Groups |> List.exists (fun g -> g.Id = groupId && g.IsMember)

// Mirrors the server's rules, which have the final say.
let private canStart (m: AppModel) (w: Workflow) =
    w.Trigger = Manual && (w.Owner.Id = m.Me.Id || (w.Enabled && isMember m w.GroupId))

let private canSeeResults (m: AppModel) (w: Workflow) =
    w.Owner.Id = m.Me.Id || (w.ResultsVisibility = GroupMembers && isMember m w.GroupId)

let private stepInfo step =
    match step with
    | SendMessage _ -> "💬", "Send a message"
    | PostToGroup _ -> "📣", "Post to a group"
    | CollectInput _ -> "📝", "Collect info in a form"
    | RequestApproval _ -> "✅", "Ask for approval"
    | Condition _ -> "🔀", "Continue only if"

// ---- Pickers ----------------------------------------------------------------------

let private select (value: string) (options: (string * string) list) (onChange: string -> unit) =
    Html.select [
        prop.className "input"
        prop.value value
        prop.onChange onChange
        prop.children [ for v, label in options do Html.option [ prop.value v; prop.text label ] ]
    ]

let private recipientPicker (m: AppModel) (label: string) recipient onChange =
    let value =
        match recipient with
        | TriggeringUser -> "trigger"
        | SpecificUser id -> string id

    field
        label
        (select
            value
            [ "trigger", "The person who started the workflow"
              for u in m.Directory do string u.Id, u.DisplayName ]
            (fun v -> onChange (if v = "trigger" then TriggeringUser else SpecificUser(int v))))

let private groupPicker (m: AppModel) (label: string) groupId onChange =
    field
        label
        (select
            (string groupId)
            [ "0", "Choose a group…"
              for g in m.Groups do string g.Id, g.Name ]
            (int >> onChange))

/// A text box with the variables available at this point; clicking one inserts it.
let private templateInput (multiline: bool) (label: string) (variables: (string * string) list) (value: string) (onChange: string -> unit) =
    Html.div [
        prop.className "field"
        prop.children [
            Html.span label
            if multiline then
                Html.textarea [ prop.className "input"; prop.value value; prop.onChange onChange ]
            else
                Html.input [ prop.className "input"; prop.value value; prop.onChange onChange ]
            Html.div [
                prop.className "chips"
                prop.children [
                    for name, description in variables do
                        Html.button [
                            prop.type'.button
                            prop.className "chip"
                            prop.title description
                            prop.text ("{{" + name + "}}")
                            prop.onClick (fun _ -> onChange (value + "{{" + name + "}}"))
                        ]
                ]
            ]
        ]
    ]

// ---- Editor -----------------------------------------------------------------------

let private triggerEditor (m: AppModel) (draft: WorkflowDraft) dispatch =
    let set trigger = dispatch (EditWorkflow { draft with Trigger = trigger })

    let kind =
        match draft.Trigger with
        | Manual -> "manual"
        | NewGroupPost _ -> "post"
        | Scheduled _ -> "schedule"

    Html.section [
        prop.className "card stack"
        prop.children [
            Html.h3 "⚡ Trigger"
            field
                "Start this workflow"
                (select
                    kind
                    [ "manual", "Manually, from the Run button"
                      "post", "When someone posts in a group"
                      "schedule", "On a schedule" ]
                    (function
                    | "post" -> set (NewGroupPost(0, ""))
                    | "schedule" -> set (Scheduled { Days = [ 1; 2; 3; 4; 5 ]; Hour = 9; Minute = 0; TimeZone = browserTimeZone () })
                    | _ -> set Manual))
            match draft.Trigger with
            | Manual -> Html.p [ prop.className "muted small"; prop.text "Anyone can run it once it's turned on." ]
            | NewGroupPost(groupId, keyword) ->
                groupPicker m "Group" groupId (fun g -> set (NewGroupPost(g, keyword)))
                field
                    "Only when the post contains (optional)"
                    (textInput keyword "e.g. incident" (fun k -> set (NewGroupPost(groupId, k))))
            | Scheduled s ->
                Html.div [
                    prop.className "field"
                    prop.children [
                        Html.span "Days"
                        Html.div [
                            prop.className "chips"
                            prop.children [
                                for d in 0..6 do
                                    let on = List.contains d s.Days

                                    Html.button [
                                        prop.type'.button
                                        prop.className (if on then "chip day on" else "chip day")
                                        prop.text Workflow.dayNames[d]
                                        prop.onClick (fun _ ->
                                            let days = if on then List.filter ((<>) d) s.Days else List.sort (d :: s.Days)
                                            set (Scheduled { s with Days = days }))
                                    ]
                            ]
                        ]
                    ]
                ]
                field
                    (sprintf "Time (%s)" s.TimeZone)
                    (Html.input [
                        prop.className "input"
                        prop.type'.time
                        prop.value (pad s.Hour + ":" + pad s.Minute)
                        prop.onChange (fun (v: string) ->
                            match v.Split ':' with
                            | [| h; mi |] -> set (Scheduled { s with Hour = int h; Minute = int mi })
                            | _ -> ())
                    ])
        ]
    ]

let private fieldsEditor (fields: FormField list) (onChange: FormField list -> unit) =
    let setField i f = onChange (fields |> List.mapi (fun j x -> if i = j then f else x))

    Html.div [
        prop.className "stack"
        prop.children [
            for i, f in List.indexed fields do
                Html.div [
                    prop.key i
                    prop.className "form-field-row"
                    prop.children [
                        Html.div [
                            prop.className "row"
                            prop.children [
                                textInput f.Label "Field label" (fun v -> setField i { f with Label = v })
                                select
                                    (match f.Kind with
                                     | ShortText -> "short"
                                     | LongText -> "long"
                                     | Dropdown _ -> "dropdown")
                                    [ "short", "Short answer"; "long", "Paragraph"; "dropdown", "Dropdown" ]
                                    (fun v ->
                                        let kind =
                                            match v, f.Kind with
                                            | "dropdown", Dropdown csv -> Dropdown csv
                                            | "dropdown", _ -> Dropdown ""
                                            | "long", _ -> LongText
                                            | _ -> ShortText

                                        setField i { f with Kind = kind })
                                Html.button [
                                    prop.type'.button
                                    prop.className "btn-icon"
                                    prop.title "Remove field"
                                    prop.text "✕"
                                    prop.onClick (fun _ -> onChange (fields |> List.indexed |> List.filter (fst >> (<>) i) |> List.map snd))
                                ]
                            ]
                        ]
                        match f.Kind with
                        | Dropdown csv -> textInput csv "Choices, separated by commas" (fun v -> setField i { f with Kind = Dropdown v })
                        | _ -> ()
                        Html.div [
                            prop.className "row spread"
                            prop.children [
                                Html.label [
                                    prop.className "check"
                                    prop.children [
                                        Html.input [
                                            prop.type'.checkbox
                                            prop.isChecked f.Required
                                            prop.onChange (fun (v: bool) -> setField i { f with Required = v })
                                        ]
                                        Html.span "Required"
                                    ]
                                ]
                                match Workflow.fieldKey f.Label with
                                | "" -> ()
                                | key -> Html.code [ prop.className "muted small"; prop.text ("{{form." + key + "}}") ]
                            ]
                        ]
                    ]
                ]
            Html.div [
                button "btn" "+ Add field" (fun () -> onChange (fields @ [ { Label = ""; Kind = ShortText; Required = true } ]))
            ]
        ]
    ]

let private stepEditor (m: AppModel) (draft: WorkflowDraft) (i: int) (step: Step) dispatch =
    let set s = dispatch (EditWorkflow { draft with Steps = draft.Steps |> List.mapi (fun j x -> if i = j then s else x) })

    let move offset =
        let j = i + offset

        if j >= 0 && j < draft.Steps.Length then
            let arr = List.toArray draft.Steps
            let tmp = arr[i]
            arr[i] <- arr[j]
            arr[j] <- tmp
            dispatch (EditWorkflow { draft with Steps = List.ofArray arr })

    let variables = Workflow.variablesBefore draft.Trigger draft.Steps i
    let icon, title = stepInfo step

    Html.section [
        prop.key (sprintf "%d-%s" i title)
        prop.className "card stack step"
        prop.children [
            Html.header [
                prop.className "row spread"
                prop.children [
                    Html.h3 (sprintf "%s Step %d · %s" icon (i + 1) title)
                    Html.div [
                        prop.className "row step-tools"
                        prop.children [
                            Html.button [ prop.type'.button; prop.className "btn-icon"; prop.title "Move up"; prop.text "↑"; prop.onClick (fun _ -> move -1) ]
                            Html.button [ prop.type'.button; prop.className "btn-icon"; prop.title "Move down"; prop.text "↓"; prop.onClick (fun _ -> move 1) ]
                            Html.button [
                                prop.type'.button
                                prop.className "btn-icon"
                                prop.title "Remove step"
                                prop.text "✕"
                                prop.onClick (fun _ ->
                                    dispatch (EditWorkflow { draft with Steps = draft.Steps |> List.indexed |> List.filter (fst >> (<>) i) |> List.map snd }))
                            ]
                        ]
                    ]
                ]
            ]
            match step with
            | SendMessage(recipient, text) ->
                recipientPicker m "Send to" recipient (fun r -> set (SendMessage(r, text)))
                templateInput true "Message" variables text (fun t -> set (SendMessage(recipient, t)))
            | PostToGroup(groupId, text) ->
                groupPicker m "Post in" groupId (fun g -> set (PostToGroup(g, text)))
                templateInput true "Post" variables text (fun t -> set (PostToGroup(groupId, t)))
            | CollectInput(assignee, formTitle, fields) ->
                recipientPicker m "Who fills it in" assignee (fun r -> set (CollectInput(r, formTitle, fields)))
                field "Form title" (textInput formTitle "e.g. Time off request" (fun t -> set (CollectInput(assignee, t, fields))))
                Html.span [ prop.className "field-heading"; prop.text "Fields" ]
                fieldsEditor fields (fun fs -> set (CollectInput(assignee, formTitle, fs)))
            | RequestApproval(approver, message) ->
                recipientPicker m "Approver" approver (fun r -> set (RequestApproval(r, message)))
                templateInput true "What they're approving" variables message (fun t -> set (RequestApproval(approver, t)))
                Html.p [ prop.className "muted small"; prop.text "If they reject it, the workflow stops and the person who started it is told." ]
            | Condition(left, op, right) ->
                templateInput false "Check" variables left (fun l -> set (Condition(l, op, right)))
                field
                    "Is"
                    (select
                        (match op with
                         | Equals -> "eq"
                         | NotEquals -> "ne"
                         | Contains -> "contains")
                        [ "eq", "equal to"; "ne", "not equal to"; "contains", "containing" ]
                        (fun v ->
                            let op =
                                match v with
                                | "ne" -> NotEquals
                                | "contains" -> Contains
                                | _ -> Equals

                            set (Condition(left, op, right))))
                field "Value" (textInput right "e.g. Vacation" (fun r -> set (Condition(left, op, r))))
                Html.p [ prop.className "muted small"; prop.text "Ignores upper and lower case. If it doesn't match, the workflow stops here." ]
        ]
    ]

let private newSteps =
    [ "💬 Send a message", SendMessage(TriggeringUser, "")
      "📣 Post to a group", PostToGroup(0, "")
      "📝 Collect info", CollectInput(TriggeringUser, "", [ { Label = ""; Kind = ShortText; Required = true } ])
      "✅ Ask for approval", RequestApproval(TriggeringUser, "")
      "🔀 Continue only if", Condition("", Equals, "") ]

let editorPage (m: AppModel) dispatch =
    let draft = m.Editor
    let validation = Workflow.validate draft

    Html.div [
        prop.className "workflow-editor"
        prop.children [
            Html.div [
                prop.className "row spread page-title"
                prop.children [
                    Html.h2 (if draft.Id = 0 then "New workflow" else "Edit workflow")
                    Html.div [
                        prop.className "row"
                        prop.children [
                            button "btn" "Cancel" (fun () -> dispatch (Navigate(WorkflowsPage AllWorkflows)))
                            Html.button [
                                prop.type'.button
                                prop.className "btn btn-primary"
                                prop.disabled (Result.isError validation || m.Saving)
                                prop.text (if m.Saving then "Saving…" else "Save")
                                prop.onClick (fun _ -> dispatch SaveWorkflow)
                            ]
                        ]
                    ]
                ]
            ]
            Html.section [
                prop.className "card stack"
                prop.children [
                    field "Name" (textInput draft.Name "e.g. Request time off" (fun v -> dispatch (EditWorkflow { draft with Name = v })))
                    field
                        "Description"
                        (textInput draft.Description "What does it do?" (fun v -> dispatch (EditWorkflow { draft with Description = v })))
                ]
            ]
            Html.section [
                prop.className "card stack"
                prop.children [
                    Html.h3 "📍 Where it lives"
                    field
                        "Group"
                        (select
                            (string draft.GroupId)
                            [ "0", "Choose a group…"
                              for g in m.Groups |> List.filter (fun g -> g.IsMember) do
                                  string g.Id, g.Name ]
                            (fun v -> dispatch (EditWorkflow { draft with GroupId = int v })))
                    Html.p [
                        prop.className "muted small"
                        prop.text "The workflow appears as a post in this group, and its members can start it."
                    ]
                    field
                        "Instructions (optional)"
                        (Html.textarea [
                            prop.className "input"
                            prop.placeholder "Anything people should know before they start it"
                            prop.value draft.Instructions
                            prop.onChange (fun (v: string) -> dispatch (EditWorkflow { draft with Instructions = v }))
                        ])
                    field
                        "Who can see the results"
                        (select
                            (match draft.ResultsVisibility with
                             | GroupMembers -> "group"
                             | OwnerOnly -> "owner")
                            [ "group", "Everyone in the group"; "owner", "Only me" ]
                            (fun v ->
                                dispatch (EditWorkflow { draft with ResultsVisibility = if v = "owner" then OwnerOnly else GroupMembers })))
                ]
            ]
            triggerEditor m draft dispatch
            for i, step in List.indexed draft.Steps do
                stepEditor m draft i step dispatch
            Html.section [
                prop.className "card add-step"
                prop.children [
                    Html.h3 "Add a step"
                    Html.div [
                        prop.className "chips"
                        prop.children [
                            for label, step in newSteps do
                                Html.button [
                                    prop.type'.button
                                    prop.className "btn"
                                    prop.text label
                                    prop.disabled (draft.Steps.Length >= Workflow.maxSteps)
                                    prop.onClick (fun _ -> dispatch (EditWorkflow { draft with Steps = draft.Steps @ [ step ] }))
                                ]
                        ]
                    ]
                ]
            ]
            Html.section [
                prop.className "card how-to-run"
                prop.children [
                    Html.h3 "How people will run it"
                    Html.p (Workflow.howToRun (groupName m) true draft.Trigger draft.Steps)
                ]
            ]
            match validation with
            | Error e -> Html.p [ prop.className "validation"; prop.text ("To save: " + e) ]
            | Ok() -> ()
        ]
    ]

// ---- List, tasks and history ------------------------------------------------------

let private tabs (m: AppModel) (current: WorkflowTab) dispatch =
    let tab t (label: string) =
        Html.button [
            prop.type'.button
            prop.className (if t = current then "tab active" else "tab")
            prop.text label
            prop.onClick (fun _ -> dispatch (Navigate(WorkflowsPage t)))
        ]

    Html.div [
        prop.className "tabs workflow-tabs"
        prop.children [
            tab AllWorkflows "Workflows"
            tab MyTasks (if m.Tasks.IsEmpty then "My tasks" else sprintf "My tasks (%d)" m.Tasks.Length)
        ]
    ]

let private workflowCard (m: AppModel) (w: Workflow) dispatch =
    let mine = w.Owner.Id = m.Me.Id

    Html.article [
        prop.key w.Id
        prop.className "card workflow-card"
        prop.children [
            Html.div [
                prop.className "row spread"
                prop.children [
                    Html.h3 w.Name
                    Html.span [
                        prop.className (if w.Enabled then "pill pill-on" else "pill")
                        prop.text (if w.Enabled then "On" else "Off")
                    ]
                ]
            ]
            if w.Description <> "" then
                Html.p [ prop.className "muted"; prop.text w.Description ]
            Html.p [ prop.className "small"; prop.text ("⚡ " + describeTrigger m w.Trigger) ]
            Html.p [
                prop.className "muted small"
                prop.text (
                    sprintf "In %s · %s%s" w.GroupName
                        (w.Steps |> List.map (stepInfo >> snd) |> String.concat " → ")
                        (if mine then "" else " · by " + w.Owner.DisplayName)
                )
            ]
            Html.div [
                prop.className "row workflow-actions"
                prop.children [
                    if canStart m w then
                        button "btn btn-primary" "Start workflow" (fun () -> dispatch (RunWorkflow w.Id))
                    if canSeeResults m w then
                        button "btn" "Results" (fun () -> dispatch (Navigate(WorkflowResultsPage w.Id)))
                    if mine then
                        button "btn" "Edit" (fun () -> dispatch (OpenWorkflowEditor w))
                        button "btn" (if w.Enabled then "Turn off" else "Turn on") (fun () -> dispatch (SetWorkflowEnabled(w.Id, not w.Enabled)))
                        Html.button [
                            prop.type'.button
                            prop.className "btn btn-danger"
                            prop.text "Delete"
                            prop.onClick (fun _ ->
                                if Browser.Dom.window.confirm (sprintf "Delete \"%s\"? Its run history is deleted too." w.Name) then
                                    dispatch (DeleteWorkflow w.Id))
                        ]
                ]
            ]
        ]
    ]

let private taskCard (m: AppModel) (t: PendingTask) dispatch =
    let answers = m.TaskAnswers |> Map.tryFind t.RunId |> Option.defaultValue Map.empty
    let answer key = answers |> Map.tryFind key |> Option.defaultValue ""
    let setAnswer key value = dispatch (SetTaskAnswer(t.RunId, key, value))

    Html.article [
        prop.key t.RunId
        prop.className "card stack task-card"
        prop.children [
            Html.div [
                prop.className "muted small"
                prop.text (
                    sprintf "⚡ %s%s · %s" t.WorkflowName
                        (match t.RequestedBy with
                         | Some u when u.Id <> m.Me.Id -> " · from " + u.DisplayName
                         | _ -> "")
                        (formatDateTime t.CreatedAt)
                )
            ]
            match t.Kind with
            | FormTask(title, fields) ->
                Html.form [
                    prop.className "stack"
                    prop.onSubmit (fun e ->
                        e.preventDefault ()
                        dispatch (SubmitTask t.RunId))
                    prop.children [
                        Html.h3 title
                        for f in fields do
                            let key = Workflow.fieldKey f.Label
                            let label = f.Label + (if f.Required then " *" else "")

                            match f.Kind with
                            | ShortText -> field label (textInput (answer key) "" (setAnswer key))
                            | LongText ->
                                field label (Html.textarea [ prop.className "input"; prop.value (answer key); prop.onChange (setAnswer key) ])
                            | Dropdown csv ->
                                field
                                    label
                                    (select (answer key) (("", "Choose…") :: (Workflow.options csv |> List.map (fun o -> o, o))) (setAnswer key))
                        Html.div [ Html.button [ prop.type'.submit; prop.className "btn btn-primary"; prop.text "Submit" ] ]
                    ]
                ]
            | ApprovalTask message ->
                Html.h3 "Approval needed"
                Html.p [ prop.className "approval-message"; prop.text message ]
                textInput (m.TaskComments |> Map.tryFind t.RunId |> Option.defaultValue "") "Comment (optional)" (fun v ->
                    dispatch (SetTaskComment(t.RunId, v)))
                Html.div [
                    prop.className "row"
                    prop.children [
                        button "btn btn-primary" "Approve" (fun () -> dispatch (DecideTask(t.RunId, true)))
                        button "btn btn-danger" "Reject" (fun () -> dispatch (DecideTask(t.RunId, false)))
                    ]
                ]
        ]
    ]

let workflowsPage (m: AppModel) (tab: WorkflowTab) dispatch =
    Html.div [
        Html.div [
            prop.className "row spread page-title"
            prop.children [ Html.h2 "Workflows"; button "btn btn-primary" "+ New workflow" (fun () -> dispatch NewWorkflow) ]
        ]
        tabs m tab dispatch
        match tab with
        | AllWorkflows ->
            if m.Workflows.IsEmpty then
                Html.p [ prop.className "empty"; prop.text "No workflows yet. Create one to automate a routine task." ]
            else
                Html.div [ for w in m.Workflows do workflowCard m w dispatch ]
        | MyTasks ->
            if m.Tasks.IsEmpty then
                Html.p [ prop.className "empty"; prop.text "Nothing waiting on you." ]
            else
                Html.div [ for t in m.Tasks do taskCard m t dispatch ]
    ]

let private statusPill (status: RunStatus) =
    let className, text =
        match status with
        | Running -> "pill", "Running"
        | WaitingForInput u -> "pill pill-wait", "Waiting for " + u.DisplayName
        | WaitingForApproval u -> "pill pill-wait", "Awaiting approval from " + u.DisplayName
        | Completed -> "pill pill-on", "Completed"
        | Stopped _ -> "pill", "Stopped"
        | Failed _ -> "pill pill-error", "Failed"

    Html.span [ prop.className className; prop.text text ]

let private statusText (status: RunStatus) =
    match status with
    | Running -> "Running"
    | WaitingForInput u -> "Waiting for " + u.DisplayName
    | WaitingForApproval u -> "Awaiting approval from " + u.DisplayName
    | Completed -> "Completed"
    | Stopped reason -> "Stopped: " + reason
    | Failed error -> "Failed: " + error

[<Emit("(() => { const a = document.createElement('a'); a.href = URL.createObjectURL(new Blob([$1], { type: 'text/csv;charset=utf-8' })); a.download = $0; a.click(); setTimeout(() => URL.revokeObjectURL(a.href), 1000) })()")>]
let private download (_fileName: string) (_contents: string) : unit = jsNative

/// Quotes a CSV cell, and defuses text a spreadsheet would run as a formula.
let private csvCell (value: string) =
    let value = if value.Length > 0 && "=+-@".Contains(string value[0]) then "'" + value else value
    "\"" + value.Replace("\"", "\"\"") + "\""

let private starter (run: WorkflowRun) =
    run.TriggeredBy |> Option.map (fun u -> u.DisplayName) |> Option.defaultValue "Schedule"

let private resultsCsv (results: WorkflowResults) =
    let columns = Workflow.resultColumns results.Workflow.Trigger results.Workflow.Steps

    [ [ "Run"; "Started by"; "Started"; "Status" ] @ (columns |> List.map snd)
      for run in results.Runs do
          [ string run.Id; starter run; formatDateTime run.StartedAt; statusText run.Status ]
          @ (columns |> List.map (fun (key, _) -> run.Values |> Map.tryFind key |> Option.defaultValue "")) ]
    |> List.map (List.map csvCell >> String.concat ",")
    |> String.concat "\r\n"

let private stat (label: string) (value: int) =
    Html.div [ prop.className "stat"; prop.children [ Html.strong (string value); Html.span label ] ]

let resultsPage (m: AppModel) (workflowId: int) dispatch =
    match m.Results with
    | Some results when results.Workflow.Id = workflowId ->
        let w = results.Workflow
        let columns = Workflow.resultColumns w.Trigger w.Steps
        let count f = results.Runs |> List.filter (fun r -> f r.Status) |> List.length

        Html.div [
            Html.div [
                prop.className "row spread page-title"
                prop.children [
                    Html.h2 (w.Name + " · results")
                    Html.div [
                        prop.className "row"
                        prop.children [
                            button "btn" "Back" (fun () -> dispatch (Navigate(Feed(GroupFeed w.GroupId))))
                            if results.CanEdit then
                                button "btn" "Edit" (fun () -> dispatch (OpenWorkflowEditor w))
                            Html.button [
                                prop.type'.button
                                prop.className "btn"
                                prop.disabled results.Runs.IsEmpty
                                prop.text "Download CSV"
                                prop.onClick (fun _ -> download (Workflow.fieldKey w.Name + "-results.csv") (resultsCsv results))
                            ]
                        ]
                    ]
                ]
            ]
            Html.section [
                prop.className "card stack"
                prop.children [
                    Html.p [ prop.className "muted small"; prop.text (sprintf "In %s · by %s" w.GroupName w.Owner.DisplayName) ]
                    if w.Description <> "" then
                        Html.p w.Description
                    Html.div [
                        prop.className "how-to-run"
                        prop.children [
                            Html.strong "How to run it"
                            Html.p (Workflow.howToRun (groupName m) w.Enabled w.Trigger w.Steps)
                            if w.Instructions <> "" then
                                Html.p [ prop.className "instructions"; prop.text w.Instructions ]
                        ]
                    ]
                    if canStart m w then
                        Html.div [ button "btn btn-primary" "Start workflow" (fun () -> dispatch (RunWorkflow w.Id)) ]
                    if w.ResultsVisibility = OwnerOnly then
                        Html.p [ prop.className "muted small"; prop.text "🔒 Only the owner can see these results." ]
                ]
            ]
            Html.div [
                prop.className "stats"
                prop.children [
                    stat "Runs" results.Runs.Length
                    stat "Completed" (count (fun s -> s = Completed))
                    stat
                        "In progress"
                        (count (function
                            | Running
                            | WaitingForInput _
                            | WaitingForApproval _ -> true
                            | _ -> false))
                    stat
                        "Stopped"
                        (count (function
                            | Stopped _
                            | Failed _ -> true
                            | _ -> false))
                ]
            ]
            if results.Runs.IsEmpty then
                Html.p [ prop.className "empty"; prop.text "No results yet. They'll appear here after the first run." ]
            else
                Html.div [
                    prop.className "card results-table-wrap"
                    prop.children [
                        Html.table [
                            prop.className "results-table"
                            prop.children [
                                Html.thead [
                                    Html.tr [
                                        Html.th "Started by"
                                        Html.th "Started"
                                        Html.th "Status"
                                        for _, heading in columns do
                                            Html.th heading
                                    ]
                                ]
                                Html.tbody [
                                    for run in results.Runs do
                                        Html.tr [
                                            prop.key run.Id
                                            prop.children [
                                                Html.td (starter run)
                                                Html.td [ prop.className "nowrap"; prop.text (formatDateTime run.StartedAt) ]
                                                Html.td [ statusPill run.Status ]
                                                for key, _ in columns do
                                                    Html.td (run.Values |> Map.tryFind key |> Option.defaultValue "")
                                            ]
                                        ]
                                ]
                            ]
                        ]
                    ]
                ]

                Html.section [
                    prop.className "card stack"
                    prop.children [
                        Html.h3 "Step by step"
                        for run in results.Runs do
                            Html.details [
                                prop.key run.Id
                                prop.className "run-details"
                                prop.children [
                                    Html.summary (
                                        sprintf "Run #%d · %s · %s · %s" run.Id (starter run) (formatDateTime run.StartedAt) (statusText run.Status)
                                    )
                                    Html.ol [
                                        prop.className "run-log"
                                        prop.children [
                                            for entry in run.Log do
                                                Html.li [
                                                    Html.span [ prop.className "muted small"; prop.text (formatDateTime entry.At) ]
                                                    Html.span entry.Text
                                                ]
                                        ]
                                    ]
                                ]
                            ]
                    ]
                ]
        ]
    | _ -> Html.p [ prop.className "empty"; prop.text "Loading results…" ]

// ---- In group feeds ----------------------------------------------------------------

/// The body of a workflow's post: what it does, how to run it, and buttons to start it or see results.
let postCard (card: WorkflowCard) dispatch =
    Html.div [
        prop.className "workflow-post"
        prop.children [
            Html.div [
                prop.className "row spread"
                prop.children [
                    Html.span [ prop.className "workflow-label"; prop.text "⚡ Workflow" ]
                    if not card.Enabled then
                        Html.span [ prop.className "pill"; prop.text "Off" ]
                ]
            ]
            Html.h3 card.Name
            if card.Description <> "" then
                Html.p card.Description
            Html.div [
                prop.className "how-to-run"
                prop.children [
                    Html.strong "How to run it"
                    Html.p card.HowToRun
                    if card.Instructions <> "" then
                        Html.p [ prop.className "instructions"; prop.text card.Instructions ]
                ]
            ]
            Html.div [
                prop.className "row workflow-actions"
                prop.children [
                    if card.CanStart then
                        button "btn btn-primary" "Start workflow" (fun () -> dispatch (RunWorkflow card.WorkflowId))
                    elif card.IsManual && card.Enabled then
                        Html.span [ prop.className "muted small"; prop.text "Join this group to start it." ]
                    if card.CanSeeResults then
                        button
                            "btn"
                            (match card.RunCount with
                             | 0 -> "Results"
                             | 1 -> "Results (1 run)"
                             | n -> sprintf "Results (%d runs)" n)
                            (fun () -> dispatch (Navigate(WorkflowResultsPage card.WorkflowId)))
                ]
            ]
        ]
    ]

/// A compact list of a group's workflows for the top of its feed.
let groupWorkflows (m: AppModel) (groupId: int) dispatch =
    let workflows = m.Workflows |> List.filter (fun w -> w.GroupId = groupId)

    if workflows.IsEmpty then
        Html.none
    else
        Html.div [
            prop.className "group-workflows"
            prop.children [
                Html.span [ prop.className "muted small"; prop.text "⚡ Workflows in this group" ]
                for w in workflows do
                    Html.div [
                        prop.key w.Id
                        prop.className "row spread group-workflow"
                        prop.children [
                            Html.div [
                                Html.strong w.Name
                                Html.div [ prop.className "muted small"; prop.text (describeTrigger m w.Trigger) ]
                            ]
                            Html.div [
                                prop.className "row"
                                prop.children [
                                    if canStart m w then
                                        button "btn btn-primary" "Start" (fun () -> dispatch (RunWorkflow w.Id))
                                    if canSeeResults m w then
                                        button "btn" "Results" (fun () -> dispatch (Navigate(WorkflowResultsPage w.Id)))
                                ]
                            ]
                        ]
                    ]
            ]
        ]
