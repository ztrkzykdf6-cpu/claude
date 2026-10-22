/// SQLite persistence for workflows and their runs.
module WorkflowDb

open System
open Microsoft.Data.Sqlite
open Newtonsoft.Json
open Fable.Remoting.Json
open Shared
open Workflows
open Db

let private schema =
    """
    CREATE TABLE IF NOT EXISTS workflows (
        id            INTEGER PRIMARY KEY AUTOINCREMENT,
        owner_id      INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,
        name          TEXT NOT NULL,
        description   TEXT NOT NULL DEFAULT '',
        enabled       INTEGER NOT NULL DEFAULT 1,
        trigger_kind  TEXT NOT NULL,
        trigger_group INTEGER,
        trigger_json  TEXT NOT NULL,
        steps_json    TEXT NOT NULL,
        last_fired_at TEXT NOT NULL,
        updated_at    TEXT NOT NULL
    );
    CREATE INDEX IF NOT EXISTS ix_workflows_trigger ON workflows(trigger_kind, trigger_group);
    CREATE TABLE IF NOT EXISTS workflow_runs (
        id           INTEGER PRIMARY KEY AUTOINCREMENT,
        workflow_id  INTEGER NOT NULL REFERENCES workflows(id) ON DELETE CASCADE,
        triggered_by INTEGER REFERENCES users(id),
        status       TEXT NOT NULL,
        detail       TEXT NOT NULL DEFAULT '',
        waiting_on   INTEGER REFERENCES users(id),
        current_step INTEGER NOT NULL DEFAULT 0,
        steps_json   TEXT NOT NULL,
        vars_json    TEXT NOT NULL,
        log_json     TEXT NOT NULL,
        created_at   TEXT NOT NULL,
        updated_at   TEXT NOT NULL
    );
    CREATE INDEX IF NOT EXISTS ix_runs_workflow ON workflow_runs(workflow_id, id);
    CREATE INDEX IF NOT EXISTS ix_runs_waiting ON workflow_runs(waiting_on, status);
    """

// Same JSON shape as the remoting API, so union cases round-trip.
let private converter = FableJsonConverter()
let private toJson (value: obj) = JsonConvert.SerializeObject(value, converter)
let private fromJson<'T> (json: string) = JsonConvert.DeserializeObject<'T>(json, converter)

let private iso (time: DateTime) = time.ToString("o", Globalization.CultureInfo.InvariantCulture)

/// Run state as stored; the engine turns it into the API's `WorkflowRun`.
type RunRow =
    { Id: int
      WorkflowId: int
      WorkflowName: string
      OwnerId: int
      TriggeredBy: User option
      /// "running", "input", "approval", "completed", "stopped" or "failed".
      Status: string
      Detail: string
      WaitingOn: User option
      CurrentStep: int
      /// A snapshot taken when the run started, so editing the workflow can't change a run midway.
      Steps: Step list
      Variables: Map<string, string>
      Log: RunLogEntry list
      CreatedAt: DateTime }

// Columns added after the first version of the table; Initialize adds them to older databases.
let private laterColumns =
    [ "group_id", "INTEGER REFERENCES work_groups(id) ON DELETE CASCADE"
      "post_id", "INTEGER REFERENCES posts(id) ON DELETE SET NULL"
      "instructions", "TEXT NOT NULL DEFAULT ''"
      "results_visibility", "TEXT NOT NULL DEFAULT 'group'" ]

let private visibilityColumn =
    function
    | GroupMembers -> "group"
    | OwnerOnly -> "owner"

let private triggerColumns trigger =
    match trigger with
    | Manual -> "manual", None
    | NewGroupPost(groupId, _) -> "group_post", Some groupId
    | Scheduled _ -> "scheduled", None

let private userCols = userColumns "u"

type WorkflowStore(db: Store) =

    let workflowSql where =
        $"SELECT w.id, w.name, w.description, w.enabled, w.trigger_json, w.steps_json, w.updated_at, w.last_fired_at,
                 w.group_id, g.name, w.instructions, w.results_visibility, {userCols}, w.post_id
          FROM workflows w
          JOIN users u ON u.id = w.owner_id
          LEFT JOIN work_groups g ON g.id = w.group_id
          {where}"

    // After the seven user columns that start at 12.
    let postIdColumn = 19

    let readWorkflow (r: SqliteDataReader) =
        { Id = r.GetInt32 0
          Name = r.GetString 1
          Description = r.GetString 2
          Enabled = r.GetInt32 3 = 1
          Trigger = fromJson<Trigger> (r.GetString 4)
          Steps = fromJson<Step list> (r.GetString 5)
          UpdatedAt = readDate r 6
          GroupId = if r.IsDBNull 8 then 0 else r.GetInt32 8
          GroupName = if r.IsDBNull 9 then "" else r.GetString 9
          Instructions = r.GetString 10
          ResultsVisibility = if r.GetString 11 = "owner" then OwnerOnly else GroupMembers
          Owner = readUserAt r 12 },
        readDate r 7

    let runSql where =
        $"SELECT r.id, r.workflow_id, w.name, w.owner_id, r.triggered_by, r.status, r.detail, r.waiting_on,
                 r.current_step, r.steps_json, r.vars_json, r.log_json, r.created_at
          FROM workflow_runs r JOIN workflows w ON w.id = r.workflow_id {where}"

    let readRun (r: SqliteDataReader) =
        let userAt i = if r.IsDBNull i then None else db.GetUser(r.GetInt32 i)

        { Id = r.GetInt32 0
          WorkflowId = r.GetInt32 1
          WorkflowName = r.GetString 2
          OwnerId = r.GetInt32 3
          TriggeredBy = userAt 4
          Status = r.GetString 5
          Detail = r.GetString 6
          WaitingOn = userAt 7
          CurrentStep = r.GetInt32 8
          Steps = fromJson<Step list> (r.GetString 9)
          Variables = fromJson<Map<string, string>> (r.GetString 10)
          Log = fromJson<RunLogEntry list> (r.GetString 11)
          CreatedAt = readDate r 12 }

    member _.Initialize() =
        use conn = db.Open()
        execute conn schema [] |> ignore

        for column, definition in laterColumns do
            addColumnIfMissing conn "workflows" column definition

    /// Returns the Workflow Bot account, creating it on first use. Its password hash never verifies.
    member _.EnsureBot() : User =
        use conn = db.Open()
        execute
            conn
            "INSERT OR IGNORE INTO users (username, password_hash, display_name, title, department, avatar_color, created_at)
             VALUES (@u, '!', 'Workflow Bot', 'Automations', '', '#5b6b8c', @now)"
            [ "@u", box botUsername; "@now", box (now ()) ]
        |> ignore

        query conn ($"SELECT {userCols} FROM users u WHERE u.username = @u") [ "@u", box botUsername ] (fun r -> readUserAt r 0)
        |> List.head

    // ---- Workflows -------------------------------------------------------------

    /// Inserts (`Id = 0`) or updates a workflow, keeping its post in its group: created with the workflow, moved
    /// when the group changes. Saving resets the schedule clock so an edit never fires a missed run.
    member _.Save(ownerId: int, draft: WorkflowDraft) : int =
        use conn = db.Open()
        use tx = conn.BeginTransaction()
        let kind, triggerGroup = triggerColumns draft.Trigger
        let stamp = now ()

        let ps =
            [ "@owner", box ownerId
              "@name", box (draft.Name.Trim())
              "@desc", box (draft.Description.Trim())
              "@instructions", box (draft.Instructions.Trim())
              "@visibility", box (visibilityColumn draft.ResultsVisibility)
              "@group", box draft.GroupId
              "@kind", box kind
              "@tgroup", box triggerGroup
              "@trigger", box (toJson draft.Trigger)
              "@steps", box (toJson draft.Steps)
              "@now", box stamp
              "@id", box draft.Id ]

        let id =
            if draft.Id = 0 then
                insert
                    conn
                    "INSERT INTO workflows (owner_id, group_id, name, description, instructions, results_visibility,
                                            trigger_kind, trigger_group, trigger_json, steps_json, last_fired_at, updated_at)
                     VALUES (@owner, @group, @name, @desc, @instructions, @visibility, @kind, @tgroup, @trigger, @steps, @now, @now)"
                    ps
            else
                execute
                    conn
                    "UPDATE workflows SET group_id = @group, name = @name, description = @desc, instructions = @instructions,
                            results_visibility = @visibility, trigger_kind = @kind, trigger_group = @tgroup,
                            trigger_json = @trigger, steps_json = @steps, last_fired_at = @now, updated_at = @now
                     WHERE id = @id AND owner_id = @owner"
                    ps
                |> ignore

                draft.Id

        let postId =
            query conn "SELECT post_id FROM workflows WHERE id = @id" [ "@id", box id ] (fun r ->
                if r.IsDBNull 0 then None else Some(r.GetInt32 0))
            |> List.tryHead
            |> Option.flatten

        match postId with
        | Some p ->
            execute conn "UPDATE posts SET group_id = @g WHERE id = @p" [ "@g", box draft.GroupId; "@p", box p ] |> ignore
        | None ->
            let p =
                insert
                    conn
                    "INSERT INTO posts (author_id, group_id, body, created_at) VALUES (@owner, @group, '', @now)"
                    ps

            execute conn "UPDATE workflows SET post_id = @p WHERE id = @id" [ "@p", box p; "@id", box id ] |> ignore

        tx.Commit()
        id

    member _.Get(id: int) : Workflow option =
        use conn = db.Open()
        query conn (workflowSql "WHERE w.id = @id") [ "@id", box id ] readWorkflow |> List.tryHead |> Option.map fst

    member _.ListFor(userId: int) : Workflow list =
        use conn = db.Open()
        query
            conn
            (workflowSql
                "WHERE w.owner_id = @me OR w.group_id IN (SELECT group_id FROM group_members WHERE user_id = @me)
                 ORDER BY g.name COLLATE NOCASE, w.name COLLATE NOCASE")
            [ "@me", box userId ]
            readWorkflow
        |> List.map fst

    /// Workflows whose post is among `postIds`, keyed by post id.
    member _.ForPosts(postIds: int list) : Map<int, Workflow> =
        if postIds.IsEmpty then
            Map.empty
        else
            use conn = db.Open()
            // ids are integers from the database, so inlining them is safe.
            let ids = postIds |> List.map string |> String.concat ","

            query conn (workflowSql $"WHERE w.post_id IN ({ids})") [] (fun r -> r.GetInt32 postIdColumn, fst (readWorkflow r))
            |> Map.ofList

    member _.IsWorkflowPost(postId: int) =
        use conn = db.Open()
        query conn "SELECT 1 FROM workflows WHERE post_id = @p" [ "@p", box postId ] ignore |> List.isEmpty |> not

    member _.RunCounts(workflowIds: int list) : Map<int, int> =
        if workflowIds.IsEmpty then
            Map.empty
        else
            use conn = db.Open()
            let ids = workflowIds |> List.map string |> String.concat ","

            query
                conn
                $"SELECT workflow_id, COUNT(*) FROM workflow_runs WHERE workflow_id IN ({ids}) GROUP BY workflow_id"
                []
                (fun r -> r.GetInt32 0, r.GetInt32 1)
            |> Map.ofList

    /// Deletes a workflow with its post and runs.
    member _.Delete(id: int) =
        use conn = db.Open()
        use tx = conn.BeginTransaction()
        execute conn "DELETE FROM posts WHERE id = (SELECT post_id FROM workflows WHERE id = @id)" [ "@id", box id ] |> ignore
        execute conn "DELETE FROM workflows WHERE id = @id" [ "@id", box id ] |> ignore
        tx.Commit()

    member _.SetEnabled(id: int, enabled: bool) =
        use conn = db.Open()
        // Turning a workflow on also resets the schedule clock, so it doesn't fire for a time that passed while off.
        execute
            conn
            "UPDATE workflows SET enabled = @e, last_fired_at = CASE WHEN @e = 1 THEN @now ELSE last_fired_at END WHERE id = @id"
            [ "@e", box (if enabled then 1 else 0); "@now", box (now ()); "@id", box id ]
        |> ignore

    /// Enabled workflows triggered by posts in a group.
    member _.ForGroupPosts(groupId: int) : Workflow list =
        use conn = db.Open()
        query
            conn
            (workflowSql "WHERE w.enabled = 1 AND w.trigger_kind = 'group_post' AND w.trigger_group = @g")
            [ "@g", box groupId ]
            readWorkflow
        |> List.map fst

    /// Enabled scheduled workflows with the time they last fired.
    member _.Scheduled() : (Workflow * DateTime) list =
        use conn = db.Open()
        query conn (workflowSql "WHERE w.enabled = 1 AND w.trigger_kind = 'scheduled'") [] readWorkflow

    member _.MarkFired(id: int, at: DateTime) =
        use conn = db.Open()
        execute conn "UPDATE workflows SET last_fired_at = @at WHERE id = @id" [ "@at", box (iso at); "@id", box id ]
        |> ignore

    // ---- Runs --------------------------------------------------------------------

    member _.CreateRun(workflow: Workflow, triggeredBy: User option, variables: Map<string, string>) : int =
        use conn = db.Open()
        let stamp = now ()
        insert
            conn
            "INSERT INTO workflow_runs (workflow_id, triggered_by, status, steps_json, vars_json, log_json, created_at, updated_at)
             VALUES (@w, @by, 'running', @steps, @vars, '[]', @now, @now)"
            [ "@w", box workflow.Id
              "@by", box (triggeredBy |> Option.map (fun u -> u.Id))
              "@steps", box (toJson workflow.Steps)
              "@vars", box (toJson variables)
              "@now", box stamp ]

    member _.GetRun(id: int) : RunRow option =
        use conn = db.Open()
        query conn (runSql "WHERE r.id = @id") [ "@id", box id ] readRun |> List.tryHead

    member _.SaveRun(run: RunRow) =
        use conn = db.Open()
        execute
            conn
            "UPDATE workflow_runs SET status = @s, detail = @d, waiting_on = @wait, current_step = @step,
                    vars_json = @vars, log_json = @log, updated_at = @now
             WHERE id = @id"
            [ "@s", box run.Status
              "@d", box run.Detail
              "@wait", box (run.WaitingOn |> Option.map (fun u -> u.Id))
              "@step", box run.CurrentStep
              "@vars", box (toJson run.Variables)
              "@log", box (toJson run.Log)
              "@now", box (now ())
              "@id", box run.Id ]
        |> ignore

    member _.RunsFor(workflowId: int) : RunRow list =
        use conn = db.Open()
        query conn (runSql "WHERE r.workflow_id = @w ORDER BY r.id DESC LIMIT 50") [ "@w", box workflowId ] readRun

    /// Paused runs waiting on a user to fill in a form or decide an approval.
    member _.WaitingOn(userId: int) : RunRow list =
        use conn = db.Open()
        query
            conn
            (runSql "WHERE r.waiting_on = @u AND r.status IN ('input', 'approval') ORDER BY r.id")
            [ "@u", box userId ]
            readRun
