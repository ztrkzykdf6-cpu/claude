/// Small view helpers shared by every page.
module Components

open System
open Feliz
open Shared
open State

let initials (name: string) =
    name.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries)
    |> Array.truncate 2
    |> Array.map (fun part -> string (Char.ToUpper part[0]))
    |> String.concat ""

let avatar (size: string) (user: User) =
    Html.div [
        prop.className ("avatar avatar-" + size)
        prop.style [ style.backgroundColor user.AvatarColor ]
        prop.title user.DisplayName
        prop.text (initials user.DisplayName)
    ]

let months =
    [| "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun"; "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec" |]

// Formatted by hand: Fable supports only a subset of .NET date format strings.
let formatDate (time: DateTime) =
    let t = time.ToLocalTime()
    sprintf "%s %d, %d" months[t.Month - 1] t.Day t.Year

let formatDateTime (time: DateTime) =
    let t = time.ToLocalTime()
    sprintf "%s %02d:%02d" (formatDate time) t.Hour t.Minute

let timeAgo (time: DateTime) =
    let elapsed = DateTime.UtcNow - time.ToUniversalTime()

    if elapsed.TotalMinutes < 1.0 then "just now"
    elif elapsed.TotalHours < 1.0 then sprintf "%dm" (int elapsed.TotalMinutes)
    elif elapsed.TotalDays < 1.0 then sprintf "%dh" (int elapsed.TotalHours)
    elif elapsed.TotalDays < 7.0 then sprintf "%dd" (int elapsed.TotalDays)
    else formatDate time

let onEnter (action: unit -> unit) =
    prop.onKeyDown (fun (e: Browser.Types.KeyboardEvent) ->
        if e.key = "Enter" && not e.shiftKey then
            e.preventDefault ()
            action ())

let button (className: string) (label: string) (onClick: unit -> unit) =
    Html.button [
        prop.type'.button
        prop.className className
        prop.text label
        prop.onClick (fun _ -> onClick ())
    ]

let field (label: string) (input: ReactElement) =
    Html.label [ prop.className "field"; prop.children [ Html.span label; input ] ]

let textInput (value: string) (placeholder: string) (onChange: string -> unit) =
    Html.input [
        prop.className "input"
        prop.value value
        prop.placeholder placeholder
        prop.onChange onChange
    ]

let userLink (user: User) dispatch =
    Html.a [
        prop.className "user-link"
        prop.href "#"
        prop.text user.DisplayName
        prop.onClick (fun e ->
            e.preventDefault ()
            dispatch (Navigate(Profile user)))
    ]
