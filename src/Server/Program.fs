module Program

open System
open System.IO
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Http
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

let private webApp (db: Db.Store) (logger: ILogger) : HttpHandler =
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
            Api.workplace db (ctx.Items[userKey] :?> User) (ctx.Items[tokenKey] :?> string))
        |> Remoting.withErrorHandler (errorHandler logger)
        |> Remoting.buildHttpHandler

    choose
        [ routeStartsWith "/api/IAuthApi/" >=> authApi
          routeStartsWith "/api/IWorkplaceApi/" >=> requireUser db >=> workplaceApi
          routeStartsWith "/api/" >=> RequestErrors.NOT_FOUND "Unknown endpoint" ]

[<EntryPoint>]
let main args =
    let builder = WebApplication.CreateBuilder(args)
    builder.Services.AddGiraffe() |> ignore

    let dbPath = builder.Configuration["Database:Path"] |> Option.ofObj |> Option.defaultValue "workplace.db"
    Path.GetDirectoryName(Path.GetFullPath dbPath) |> Directory.CreateDirectory |> ignore

    let db = Db.Store($"Data Source={dbPath}")
    db.Initialize()

    if builder.Configuration.GetValue<bool>("Database:Seed", true) then
        Seed.run db

    let app = builder.Build()
    let logger = app.Services.GetRequiredService<ILoggerFactory>().CreateLogger("Workplace")

    // The production client build is copied into wwwroot; in development Vite serves it instead.
    app.UseDefaultFiles().UseStaticFiles() |> ignore
    app.UseGiraffe(webApp db logger)
    app.Run()
    0
