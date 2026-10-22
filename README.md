# Workplace

A team collaboration app in the style of Workplace from Meta, written end to end in F#.

- **Home feed**: post updates, like posts and comment on them
- **Groups**: create, join and leave groups, each with its own feed
- **Chat**: one-to-one messages delivered instantly over SignalR, with an unread badge for other chats
- **People**: a searchable company directory with editable profiles
- **Accounts**: registration and sign-in with PBKDF2-hashed passwords and bearer session tokens

## Stack

| Layer | Tech |
| --- | --- |
| Server | ASP.NET Core + Giraffe, SQLite (`Microsoft.Data.Sqlite`), SignalR for live chat |
| API | [Fable.Remoting](https://github.com/Zaid-Ajaj/Fable.Remoting): type-safe calls declared once in `src/Shared` |
| Client | [Fable](https://fable.io) 4 → JavaScript, Elmish (MVU), Feliz (React 18), Vite |

```
src/
  Shared/   Domain types and the API contract used by both sides
  Server/   Db.fs (SQLite), Auth.fs (passwords and tokens), Realtime.fs (SignalR hub), Api.fs, Seed.fs, Program.fs
  Client/   Api.fs (proxies), Realtime.fs (SignalR connection), State.fs (Elmish model/update), View.fs (Feliz), styles.css
```

## Prerequisites

- .NET SDK 10
- Node.js 20 or newer

```bash
dotnet tool restore
npm install
```

## Development

Run the server and the client in two terminals:

```bash
dotnet watch run --project src/Server   # API on http://localhost:5000
npm start                               # Fable + Vite on http://localhost:8080; proxies /api and /hubs to the server
```

Open http://localhost:8080. On first run the database is seeded with demo data. You can sign in as
`alex`, `priya`, `sam`, `mei` or `jordan` with the password `password123`, or create an account.

## Production build

```bash
npm run build                           # compiles the client into src/Server/wwwroot
dotnet publish src/Server -c Release -o out
cd out && dotnet Server.dll             # serves the app and the API on one port
```

## Configuration

`src/Server/appsettings.json`, or environment variables such as `Database__Path`:

| Setting | Default | Meaning |
| --- | --- | --- |
| `Database:Path` | `workplace.db` | SQLite file location |
| `Database:Seed` | `true` | Add demo users, groups and posts when the database is empty |

Turn off seeding (`Database__Seed=false`) for a real deployment, because the demo accounts share a known password.

## Notes

- Groups are open: anyone can read a group's feed, and only members can post in it.
- Messages are sent through the regular API and pushed to both people over the `/hubs/chat` SignalR hub. The
  hub authenticates with the session token in the `access_token` query string, because browsers can't set headers
  on WebSockets. After a dropped connection, the client reconnects and fetches anything it missed.
- The hub runs in-process, so live delivery reaches only clients connected to the same server instance. Scaling
  out to several instances needs a SignalR backplane (such as Redis) or Azure SignalR Service.
- The client stays on Fable 4 because Fable.Remoting's JSON decoder doesn't yet handle Fable 5's reflection info for lists and options.
