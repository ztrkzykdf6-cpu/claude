/// Demo content for an empty database, so the app has something to show on first run.
module Seed

open Shared
open Workflows
open Db

let demoPassword = "password123"

let private people: RegisterRequest list =
    [ { Username = "alex"; Password = demoPassword; DisplayName = "Alex Rivera"; Title = "Engineering Manager"; Department = "Engineering" }
      { Username = "priya"; Password = demoPassword; DisplayName = "Priya Natarajan"; Title = "Product Designer"; Department = "Design" }
      { Username = "sam"; Password = demoPassword; DisplayName = "Sam Okafor"; Title = "Backend Engineer"; Department = "Engineering" }
      { Username = "mei"; Password = demoPassword; DisplayName = "Mei Chen"; Title = "People Partner"; Department = "HR" }
      { Username = "jordan"; Password = demoPassword; DisplayName = "Jordan Blake"; Title = "Sales Lead"; Department = "Sales" } ]

let run (db: Store) (workflows: WorkflowDb.WorkflowStore) =
    if db.UserCount() = 0 then
        let hash = Auth.hashPassword demoPassword
        let users = people |> List.map (fun p -> db.CreateUser(p, hash) |> Result.toOption |> Option.get)
        let id name = (users |> List.find (fun u -> u.Username = name)).Id

        let groupId owner name description =
            match db.CreateGroup(id owner, { Name = name; Description = description }) with
            | Ok g -> g
            | Error e -> failwith e

        let everyone = groupId "mei" "Company Announcements" "Official news and updates for everyone."
        let eng = groupId "alex" "Engineering" "Design reviews, incidents and release notes."
        let social = groupId "jordan" "Coffee & Hobbies" "Off-topic chat, pets, weekend plans."

        for u in users do
            db.SetMembership(u.Id, everyone, true)

        for name in [ "sam"; "priya" ] do
            db.SetMembership(id name, eng, true)

        for name in [ "priya"; "mei"; "sam" ] do
            db.SetMembership(id name, social, true)

        let welcome =
            db.CreatePost(
                id "mei",
                Some everyone,
                "Welcome to our new workplace! Use groups for teams and projects, chat for quick questions, and the People tab to find colleagues."
            )

        db.AddComment(id "alex", welcome, "Great to have everything in one place.")
        db.ToggleLike(id "alex", welcome)
        db.ToggleLike(id "priya", welcome)

        let release = db.CreatePost(id "sam", Some eng, "Release 2.4 is out: faster search, and the export bug is fixed.")
        db.ToggleLike(id "alex", release)

        db.CreatePost(id "priya", Some social, "Who's up for a board-game lunch on Friday?") |> ignore
        db.CreatePost(id "jordan", None, "Closed our biggest deal of the quarter. Thanks to everyone who helped!") |> ignore

        db.SendMessage(id "alex", id "sam", "Can you look at the deploy checklist before tomorrow?") |> ignore
        db.SendMessage(id "sam", id "alex", "On it, I'll send comments by noon.") |> ignore

        let workflow owner group name description instructions visibility trigger steps =
            workflows.Save(
                id owner,
                { Id = 0
                  GroupId = group
                  Name = name
                  Description = description
                  Instructions = instructions
                  ResultsVisibility = visibility
                  Trigger = trigger
                  Steps = steps }
            )
            |> ignore

        workflow
            "mei"
            everyone
            "Request time off"
            "Ask for time off; your manager approves it."
            "Request at least two weeks ahead for vacations. For sick leave, request on the day."
            OwnerOnly
            Manual
            [ CollectInput(
                  TriggeringUser,
                  "Time off request",
                  [ { Label = "Dates"; Kind = ShortText; Required = true }
                    { Label = "Type"; Kind = Dropdown "Vacation, Sick leave, Personal"; Required = true }
                    { Label = "Notes"; Kind = LongText; Required = false } ]
              )
              RequestApproval(
                  SpecificUser(id "alex"),
                  "{{trigger.user}} is asking for time off ({{form.type}}): {{form.dates}}\n{{form.notes}}"
              )
              SendMessage(TriggeringUser, "Your time off for {{form.dates}} was approved by {{approval.by}}. Enjoy!") ]

        workflow
            "alex"
            eng
            "Incident alert"
            "Messages the engineering manager when an Engineering post mentions an incident."
            "Mention \"incident\" in your post and include what's affected."
            GroupMembers
            (NewGroupPost(eng, "incident"))
            [ SendMessage(SpecificUser(id "alex"), "{{trigger.user}} posted about an incident in {{trigger.group}}:\n{{trigger.post}}") ]

        workflow
            "mei"
            everyone
            "Monday kickoff"
            "Posts a weekly prompt in Company Announcements."
            ""
            GroupMembers
            (Scheduled { Days = [ 1 ]; Hour = 9; Minute = 0; TimeZone = "UTC" })
            [ PostToGroup(everyone, "Happy Monday! What's your top priority this week? Share it in the comments.") ]
