/// SQLite persistence: schema, row mapping and every query the API needs.
module Db

open System
open System.Globalization
open Microsoft.Data.Sqlite
open Shared

let private schema =
    """
    CREATE TABLE IF NOT EXISTS users (
        id            INTEGER PRIMARY KEY AUTOINCREMENT,
        username      TEXT NOT NULL UNIQUE COLLATE NOCASE,
        password_hash TEXT NOT NULL,
        display_name  TEXT NOT NULL,
        title         TEXT NOT NULL DEFAULT '',
        department    TEXT NOT NULL DEFAULT '',
        bio           TEXT NOT NULL DEFAULT '',
        avatar_color  TEXT NOT NULL,
        created_at    TEXT NOT NULL
    );
    CREATE TABLE IF NOT EXISTS sessions (
        token_hash TEXT PRIMARY KEY,
        user_id    INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,
        created_at TEXT NOT NULL
    );
    CREATE TABLE IF NOT EXISTS work_groups (
        id          INTEGER PRIMARY KEY AUTOINCREMENT,
        name        TEXT NOT NULL UNIQUE COLLATE NOCASE,
        description TEXT NOT NULL DEFAULT '',
        created_by  INTEGER NOT NULL REFERENCES users(id),
        created_at  TEXT NOT NULL
    );
    CREATE TABLE IF NOT EXISTS group_members (
        group_id INTEGER NOT NULL REFERENCES work_groups(id) ON DELETE CASCADE,
        user_id  INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,
        PRIMARY KEY (group_id, user_id)
    );
    CREATE TABLE IF NOT EXISTS posts (
        id         INTEGER PRIMARY KEY AUTOINCREMENT,
        author_id  INTEGER NOT NULL REFERENCES users(id),
        group_id   INTEGER REFERENCES work_groups(id) ON DELETE CASCADE,
        body       TEXT NOT NULL,
        created_at TEXT NOT NULL
    );
    CREATE INDEX IF NOT EXISTS ix_posts_group ON posts(group_id, id);
    CREATE TABLE IF NOT EXISTS comments (
        id         INTEGER PRIMARY KEY AUTOINCREMENT,
        post_id    INTEGER NOT NULL REFERENCES posts(id) ON DELETE CASCADE,
        author_id  INTEGER NOT NULL REFERENCES users(id),
        body       TEXT NOT NULL,
        created_at TEXT NOT NULL
    );
    CREATE INDEX IF NOT EXISTS ix_comments_post ON comments(post_id, id);
    CREATE TABLE IF NOT EXISTS likes (
        post_id INTEGER NOT NULL REFERENCES posts(id) ON DELETE CASCADE,
        user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,
        PRIMARY KEY (post_id, user_id)
    );
    CREATE TABLE IF NOT EXISTS messages (
        id           INTEGER PRIMARY KEY AUTOINCREMENT,
        sender_id    INTEGER NOT NULL REFERENCES users(id),
        recipient_id INTEGER NOT NULL REFERENCES users(id),
        body         TEXT NOT NULL,
        sent_at      TEXT NOT NULL
    );
    CREATE INDEX IF NOT EXISTS ix_messages_pair ON messages(sender_id, recipient_id, id);
    """

let private avatarPalette =
    [| "#1877f2"; "#42b72a"; "#f02849"; "#8a3ffc"; "#f7b928"; "#0f9d8a"; "#e4712b"; "#5b6b8c" |]

/// The account workflow messages and posts come from. The hyphen keeps it out of the registrable namespace.
let botUsername = "workflow-bot"

let avatarColorFor (username: string) =
    let hash = username.ToLowerInvariant() |> Seq.fold (fun acc c -> (acc * 31 + int c) &&& 0x7fffffff) 7
    avatarPalette[hash % avatarPalette.Length]

let now () = DateTime.UtcNow.ToString("o", CultureInfo.InvariantCulture)

let readDate (r: SqliteDataReader) (i: int) =
    DateTime.Parse(r.GetString i, CultureInfo.InvariantCulture, DateTimeStyles.RoundtripKind)

let private dbValue (value: obj) =
    match value with
    | null -> box DBNull.Value
    | :? option<int> as o -> (match o with Some v -> box v | None -> box DBNull.Value)
    | v -> v

let private command (conn: SqliteConnection) sql (ps: (string * obj) list) =
    let cmd = conn.CreateCommand()
    cmd.CommandText <- sql
    for name, value in ps do
        cmd.Parameters.AddWithValue(name, dbValue value) |> ignore
    cmd

let query conn sql ps (read: SqliteDataReader -> 'T) =
    use cmd = command conn sql ps
    use r = cmd.ExecuteReader()
    [ while r.Read() do yield read r ]

let execute conn sql ps =
    use cmd = command conn sql ps
    cmd.ExecuteNonQuery()

/// Adds a column to an existing table if an older database doesn't have it yet.
let addColumnIfMissing (conn: SqliteConnection) (table: string) (column: string) (definition: string) =
    let exists =
        query conn $"SELECT 1 FROM pragma_table_info('{table}') WHERE name = @c" [ "@c", box column ] ignore
        |> List.isEmpty
        |> not

    if not exists then
        execute conn $"ALTER TABLE {table} ADD COLUMN {column} {definition}" [] |> ignore

let insert conn sql ps =
    use cmd = command conn (sql + "; SELECT last_insert_rowid();") ps
    cmd.ExecuteScalar() :?> int64 |> int

/// Columns a user row is read from, in the order `readUserAt` expects.
let userColumns alias =
    [ "id"; "username"; "display_name"; "title"; "department"; "bio"; "avatar_color" ]
    |> List.map (fun c -> alias + "." + c)
    |> String.concat ", "

let private userCols = userColumns "u"

let readUserAt (r: SqliteDataReader) offset : User =
    { Id = r.GetInt32 offset
      Username = r.GetString(offset + 1)
      DisplayName = r.GetString(offset + 2)
      Title = r.GetString(offset + 3)
      Department = r.GetString(offset + 4)
      Bio = r.GetString(offset + 5)
      AvatarColor = r.GetString(offset + 6) }

let private readMessage (r: SqliteDataReader) offset : ChatMessage =
    { Id = r.GetInt32 offset
      SenderId = r.GetInt32(offset + 1)
      RecipientId = r.GetInt32(offset + 2)
      Body = r.GetString(offset + 3)
      SentAt = readDate r (offset + 4) }

type Store(connectionString: string) =

    member _.Open() =
        let conn = new SqliteConnection(connectionString)
        conn.Open()
        execute conn "PRAGMA foreign_keys = ON;" [] |> ignore
        conn

    member this.Initialize() =
        use conn = this.Open()
        execute conn "PRAGMA journal_mode = WAL;" [] |> ignore
        execute conn schema [] |> ignore

    // ---- Users & sessions ----------------------------------------------------

    member this.UserCount() =
        use conn = this.Open()
        query conn "SELECT COUNT(*) FROM users" [] (fun r -> r.GetInt32 0) |> List.head

    member this.CreateUser(req: RegisterRequest, passwordHash: string) : Result<User, string> =
        use conn = this.Open()
        let exists =
            query conn "SELECT 1 FROM users WHERE username = @u" [ "@u", box req.Username ] ignore
            |> List.isEmpty
            |> not

        if exists then
            Error "That username is already taken"
        else
            let color = avatarColorFor req.Username
            let id =
                insert
                    conn
                    "INSERT INTO users (username, password_hash, display_name, title, department, avatar_color, created_at)
                     VALUES (@u, @p, @d, @t, @dep, @c, @now)"
                    [ "@u", box req.Username
                      "@p", box passwordHash
                      "@d", box req.DisplayName
                      "@t", box req.Title
                      "@dep", box req.Department
                      "@c", box color
                      "@now", box (now ()) ]

            Ok
                { Id = id
                  Username = req.Username
                  DisplayName = req.DisplayName
                  Title = req.Title
                  Department = req.Department
                  Bio = ""
                  AvatarColor = color }

    /// The user and their stored password hash, for login.
    member this.FindCredentials(username: string) =
        use conn = this.Open()
        query
            conn
            ($"SELECT {userCols}, u.password_hash FROM users u WHERE u.username = @u")
            [ "@u", box username ]
            (fun r -> readUserAt r 0, r.GetString 7)
        |> List.tryHead

    member this.GetUser(id: int) =
        use conn = this.Open()
        query conn ($"SELECT {userCols} FROM users u WHERE u.id = @id") [ "@id", box id ] (fun r -> readUserAt r 0)
        |> List.tryHead

    member this.SearchUsers(term: string) =
        use conn = this.Open()
        let pattern = "%" + term.Trim().Replace("%", "").Replace("_", "") + "%"
        query
            conn
            ($"SELECT {userCols} FROM users u
               WHERE (u.display_name LIKE @q OR u.username LIKE @q OR u.title LIKE @q OR u.department LIKE @q)
                 AND u.username <> @bot
               ORDER BY u.display_name LIMIT 100")
            [ "@q", box pattern; "@bot", box botUsername ]
            (fun r -> readUserAt r 0)

    member this.UpdateProfile(userId: int, p: ProfileUpdate) =
        use conn = this.Open()
        execute
            conn
            "UPDATE users SET display_name = @d, title = @t, department = @dep, bio = @b WHERE id = @id"
            [ "@d", box p.DisplayName; "@t", box p.Title; "@dep", box p.Department; "@b", box p.Bio; "@id", box userId ]
        |> ignore

    member this.CreateSession(tokenHash: string, userId: int) =
        use conn = this.Open()
        execute
            conn
            "INSERT INTO sessions (token_hash, user_id, created_at) VALUES (@t, @u, @now)"
            [ "@t", box tokenHash; "@u", box userId; "@now", box (now ()) ]
        |> ignore

    member this.UserForSession(tokenHash: string) =
        use conn = this.Open()
        query
            conn
            ($"SELECT {userCols} FROM sessions s JOIN users u ON u.id = s.user_id WHERE s.token_hash = @t")
            [ "@t", box tokenHash ]
            (fun r -> readUserAt r 0)
        |> List.tryHead

    member this.DeleteSession(tokenHash: string) =
        use conn = this.Open()
        execute conn "DELETE FROM sessions WHERE token_hash = @t" [ "@t", box tokenHash ] |> ignore

    // ---- Groups --------------------------------------------------------------

    member private _.GroupsWhere(conn, me: int, whereClause: string, ps) =
        query
            conn
            ($"SELECT g.id, g.name, g.description,
                      (SELECT COUNT(*) FROM group_members m WHERE m.group_id = g.id),
                      EXISTS (SELECT 1 FROM group_members m WHERE m.group_id = g.id AND m.user_id = @me)
               FROM work_groups g {whereClause} ORDER BY g.name")
            (("@me", box me) :: ps)
            (fun r ->
                { Id = r.GetInt32 0
                  Name = r.GetString 1
                  Description = r.GetString 2
                  MemberCount = r.GetInt32 3
                  IsMember = r.GetInt32 4 = 1 })

    member this.GetGroups(me: int) : Group list =
        use conn = this.Open()
        this.GroupsWhere(conn, me, "", [])

    member this.GetGroup(me: int, groupId: int) : Group option =
        use conn = this.Open()
        this.GroupsWhere(conn, me, "WHERE g.id = @g", [ "@g", box groupId ]) |> List.tryHead

    member this.CreateGroup(me: int, g: NewGroup) : Result<int, string> =
        use conn = this.Open()
        let taken =
            query conn "SELECT 1 FROM work_groups WHERE name = @n" [ "@n", box g.Name ] ignore |> List.isEmpty |> not

        if taken then
            Error "A group with that name already exists"
        else
            use tx = conn.BeginTransaction()
            let id =
                insert
                    conn
                    "INSERT INTO work_groups (name, description, created_by, created_at) VALUES (@n, @d, @me, @now)"
                    [ "@n", box g.Name; "@d", box g.Description; "@me", box me; "@now", box (now ()) ]

            execute conn "INSERT INTO group_members (group_id, user_id) VALUES (@g, @me)" [ "@g", box id; "@me", box me ]
            |> ignore

            tx.Commit()
            Ok id

    member this.SetMembership(me: int, groupId: int, isMember: bool) =
        use conn = this.Open()
        let sql =
            if isMember then
                "INSERT OR IGNORE INTO group_members (group_id, user_id) SELECT id, @me FROM work_groups WHERE id = @g"
            else
                "DELETE FROM group_members WHERE group_id = @g AND user_id = @me"

        execute conn sql [ "@g", box groupId; "@me", box me ] |> ignore

    member this.IsMember(me: int, groupId: int) =
        use conn = this.Open()
        query conn "SELECT 1 FROM group_members WHERE group_id = @g AND user_id = @me" [ "@g", box groupId; "@me", box me ] ignore
        |> List.isEmpty
        |> not

    // ---- Posts, comments, likes ---------------------------------------------

    member private _.LoadPosts(conn, me: int, whereClause: string, ps) : Post list =
        let posts =
            query
                conn
                ($"SELECT p.id, p.group_id, g.name, p.body, p.created_at,
                          (SELECT COUNT(*) FROM likes l WHERE l.post_id = p.id),
                          EXISTS (SELECT 1 FROM likes l WHERE l.post_id = p.id AND l.user_id = @me),
                          {userCols}
                   FROM posts p
                   JOIN users u ON u.id = p.author_id
                   LEFT JOIN work_groups g ON g.id = p.group_id
                   {whereClause}
                   ORDER BY p.id DESC LIMIT 50")
                (("@me", box me) :: ps)
                (fun r ->
                    { Id = r.GetInt32 0
                      GroupId = if r.IsDBNull 1 then None else Some(r.GetInt32 1)
                      GroupName = if r.IsDBNull 2 then None else Some(r.GetString 2)
                      Body = r.GetString 3
                      CreatedAt = readDate r 4
                      LikeCount = r.GetInt32 5
                      LikedByMe = r.GetInt32 6 = 1
                      Author = readUserAt r 7
                      Comments = []
                      Workflow = None })

        if posts.IsEmpty then
            []
        else
            let ids = posts |> List.map (fun p -> string p.Id) |> String.concat ","
            // ids are integers produced by the database, so inlining them is safe.
            let comments =
                query
                    conn
                    ($"SELECT c.post_id, c.id, c.body, c.created_at, {userCols}
                       FROM comments c JOIN users u ON u.id = c.author_id
                       WHERE c.post_id IN ({ids}) ORDER BY c.id")
                    []
                    (fun r ->
                        r.GetInt32 0,
                        { Id = r.GetInt32 1
                          Body = r.GetString 2
                          CreatedAt = readDate r 3
                          Author = readUserAt r 4 })
                |> List.groupBy fst
                |> Map.ofList

            posts
            |> List.map (fun p ->
                { p with
                    Comments = comments |> Map.tryFind p.Id |> Option.map (List.map snd) |> Option.defaultValue [] })

    member this.GetFeed(me: int, feed: FeedQuery) =
        use conn = this.Open()
        match feed with
        | HomeFeed ->
            this.LoadPosts(
                conn,
                me,
                "WHERE p.group_id IS NULL OR p.group_id IN (SELECT group_id FROM group_members WHERE user_id = @me)",
                []
            )
        | GroupFeed groupId -> this.LoadPosts(conn, me, "WHERE p.group_id = @g", [ "@g", box groupId ])

    member this.GetPost(me: int, postId: int) =
        use conn = this.Open()
        this.LoadPosts(conn, me, "WHERE p.id = @p", [ "@p", box postId ]) |> List.tryHead

    member this.CreatePost(me: int, groupId: int option, body: string) =
        use conn = this.Open()
        insert
            conn
            "INSERT INTO posts (author_id, group_id, body, created_at) VALUES (@me, @g, @b, @now)"
            [ "@me", box me; "@g", box groupId; "@b", box body; "@now", box (now ()) ]

    member this.PostAuthor(postId: int) =
        use conn = this.Open()
        query conn "SELECT author_id FROM posts WHERE id = @p" [ "@p", box postId ] (fun r -> r.GetInt32 0)
        |> List.tryHead

    member this.DeletePost(postId: int) =
        use conn = this.Open()
        execute conn "DELETE FROM posts WHERE id = @p" [ "@p", box postId ] |> ignore

    member this.ToggleLike(me: int, postId: int) =
        use conn = this.Open()
        let removed =
            execute conn "DELETE FROM likes WHERE post_id = @p AND user_id = @me" [ "@p", box postId; "@me", box me ]

        if removed = 0 then
            execute
                conn
                "INSERT INTO likes (post_id, user_id) SELECT id, @me FROM posts WHERE id = @p"
                [ "@p", box postId; "@me", box me ]
            |> ignore

    member this.AddComment(me: int, postId: int, body: string) =
        use conn = this.Open()
        execute
            conn
            "INSERT INTO comments (post_id, author_id, body, created_at) SELECT id, @me, @b, @now FROM posts WHERE id = @p"
            [ "@p", box postId; "@me", box me; "@b", box body; "@now", box (now ()) ]
        |> ignore

    // ---- Chat ----------------------------------------------------------------

    member this.GetConversations(me: int) : Conversation list =
        use conn = this.Open()
        query
            conn
            ($"WITH conv AS (
                   SELECT CASE WHEN sender_id = @me THEN recipient_id ELSE sender_id END AS partner_id,
                          MAX(id) AS last_id
                   FROM messages WHERE sender_id = @me OR recipient_id = @me
                   GROUP BY partner_id)
               SELECT {userCols}, m.id, m.sender_id, m.recipient_id, m.body, m.sent_at
               FROM conv
               JOIN users u ON u.id = conv.partner_id
               JOIN messages m ON m.id = conv.last_id
               ORDER BY m.id DESC")
            [ "@me", box me ]
            (fun r -> { Partner = readUserAt r 0; LastMessage = Some(readMessage r 7) })

    member this.GetMessages(me: int, partner: int, afterId: int) =
        use conn = this.Open()
        query
            conn
            "SELECT * FROM (
                 SELECT id, sender_id, recipient_id, body, sent_at FROM messages
                 WHERE id > @after
                   AND ((sender_id = @me AND recipient_id = @p) OR (sender_id = @p AND recipient_id = @me))
                 ORDER BY id DESC LIMIT 200)
             ORDER BY id"
            [ "@me", box me; "@p", box partner; "@after", box afterId ]
            (fun r -> readMessage r 0)

    member this.SendMessage(me: int, recipient: int, body: string) : ChatMessage =
        use conn = this.Open()
        let sentAt = DateTime.UtcNow
        let id =
            insert
                conn
                "INSERT INTO messages (sender_id, recipient_id, body, sent_at) VALUES (@me, @r, @b, @at)"
                [ "@me", box me
                  "@r", box recipient
                  "@b", box body
                  "@at", box (sentAt.ToString("o", CultureInfo.InvariantCulture)) ]

        { Id = id
          SenderId = me
          RecipientId = recipient
          Body = body
          SentAt = sentAt }
