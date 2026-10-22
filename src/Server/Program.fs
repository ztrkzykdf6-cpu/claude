module Program

open System
open System.IO
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.SignalR
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Logging
open Giraffe
open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Shared

let private userKey = "workplace.user"
let private tokenKey = "workplace.token"

let private errorHandler (logger: ILogger) (ex: exn) (route: RouteInfo<HttpContext>) =
    match ex with
    | Api.NotFound message -> Propagate message
    | _ ->
        logger.LogError(ex, "Unhandled error in {Method}", route.methodName)
        Ignore

/// Resolves the bearer token to a user, or answers 401.
let private requireUser (db: Db.Store) : HttpHandler =
    fun next ctx ->
        let session =
            Auth.bearerToken (string ctx.Request.Headers.Authorization)
            |> Option.bind (fun token -> db.UserForSession(Auth.hashToken token) |> Option.map (fun u -> u, token))

        match session with
        | Some(user, token) ->
            ctx.Items[userKey] <- user
            ctx.Items[tokenKey] <- token
            next ctx
        | None -> RequestErrors.UNAUTHORIZED "Bearer" "workplace" "Sign in required" next ctx

let private webApp
    (db: Db.Store)
    (store: WorkflowDb.WorkflowStore)
    (engine: WorkflowEngine.Engine)
    (hub: IHubContext<Realtime.ChatHub>)
    (logger: ILogger)
    : HttpHandler =
    let onPostCreated post =
        async {
            try
                do! engine.OnPostCreated post
            with ex ->
                logger.LogError(ex, "Starting workflows for post {PostId} failed", post.Id)
        }

    let hooks: Api.WorkflowHooks =
        { OnPostCreated = onPostCreated
          Decorate = WorkflowApi.decoratePosts db store
          IsWorkflowPost = store.IsWorkflowPost
          BotId = engine.Bot.Id }

    let authApi =
        Remoting.createApi ()
        |> Remoting.withRouteBuilder Route.builder
        |> Remoting.fromValue (Api.auth db)
        |> Remoting.withErrorHandler (errorHandler logger)
        |> Remoting.buildHttpHandler

    let workplaceApi =
        Remoting.createApi ()
        |> Remoting.withRouteBuilder Route.builder
        |> Remoting.fromContext (fun (ctx: HttpContext) ->
            Api.workplace
                db
                (Realtime.notifyMessage hub)
                hooks
                (ctx.Items[userKey] :?> User)
                (ctx.Items[tokenKey] :?> string))
        |> Remoting.withErrorHandler (errorHandler logger)
        |> Remoting.buildHttpHandler

    let workflowApi =
        Remoting.createApi ()
        |> Remoting.withRouteBuilder Route.builder
        |> Remoting.fromContext (fun (ctx: HttpContext) -> WorkflowApi.create db store engine (ctx.Items[userKey] :?> User))
        |> Remoting.withErrorHandler (errorHandler logger)
        |> Remoting.buildHttpHandler

    choose
        [ routeStartsWith "/api/IAuthApi/" >=> authApi
          routeStartsWith "/api/IWorkplaceApi/" >=> requireUser db >=> workplaceApi
          routeStartsWith "/api/IWorkflowApi/" >=> requireUser db >=> workflowApi
          routeStartsWith "/api/" >=> RequestErrors.NOT_FOUND "Unknown endpoint" ]

[<EntryPoint>]
let main args =
    let builder = WebApplication.CreateBuilder(args)
    builder.Services.AddGiraffe() |> ignore
    builder.Services.AddSignalR() |> ignore

    let dbPath = builder.Configuration["Database:Path"] |> Option.ofObj |> Option.defaultValue "workplace.db"
    Path.GetDirectoryName(Path.GetFullPath dbPath) |> Directory.CreateDirectory |> ignore

    let db = Db.Store($"Data Source={dbPath}")
    db.Initialize()

    let store = WorkflowDb.WorkflowStore(db)
    store.Initialize()

    if builder.Configuration.GetValue<bool>("Database:Seed", true) then
        Seed.run db store

    let bot = store.EnsureBot()
    builder.Services.AddSingleton<Db.Store>(db) |> ignore

    builder.Services.AddSingleton<WorkflowEngine.Engine>(fun (sp: IServiceProvider) ->
        let hub = sp.GetRequiredService<IHubContext<Realtime.ChatHub>>()

        WorkflowEngine.Engine(
            db,
            store,
            bot,
            Realtime.notifyMessage hub,
            Realtime.notifyTasks hub,
            sp.GetRequiredService<ILoggerFactory>().CreateLogger("Workflows")
        ))
    |> ignore

    builder.Services.AddHostedService<WorkflowEngine.Scheduler>() |> ignore

    let app = builder.Build()
    let logger = app.Services.GetRequiredService<ILoggerFactory>().CreateLogger("Workplace")

    // The production client build is copied into wwwroot; in development Vite serves it instead.
    app.UseDefaultFiles().UseStaticFiles() |> ignore
    let hub = app.Services.GetRequiredService<IHubContext<Realtime.ChatHub>>()
    // Giraffe passes unmatched requests on, so /hubs/chat reaches the endpoint below.
    let engine = app.Services.GetRequiredService<WorkflowEngine.Engine>()
    app.UseGiraffe(webApp db store engine hub logger)
    app.MapHub<Realtime.ChatHub>("/hubs/chat") |> ignore
    app.Run()
    0
