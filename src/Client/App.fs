module App

open Elmish
open Elmish.React

Program.mkProgram State.init State.update View.view
|> Program.withSubscription State.subscribe
|> Program.withReactSynchronous "root"
|> Program.run
