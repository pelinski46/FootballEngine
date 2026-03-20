# FootballEngine

A Football Manager-style game built in F#, Avalonia FuncUI, and Elmish. Work in progress.

## Screenshots

![Home Board](screenshots/1.png)
![Tactics Board](screenshots/2.png)
![Squad Simulation](screenshots/3.png)
![Match Lab Simulation](screenshots/4.png)

## Stack

F# · Avalonia FuncUI · Elmish · SQLite · FSharp.Stats

## What's inside

### Match Engine
Event-driven simulation using a priority queue. Each match runs a full 90-minute loop with duels, fatigue, momentum, shots, cards, injuries, and substitutions. Players move dynamically based on ball position and tactical role. Supports fast simulation (batch) and full replay with snapshots.

### Player Model
Players have 30+ attributes across physical, technical, mental, and goalkeeper categories, plus condition, morale, match fitness, and contract data. Stats evolve each season based on age curves and potential using probabilistic development.

### World Simulation
Parallel fixture simulation across multiple leagues. Standings, morale updates, and season progression run automatically. Transfer market with multi-step negotiation, watchlists, and offer history.

### Tactics
Formation builder with drag-and-drop lineup management. Formations affect player base positions on the pitch during simulation.

### Leagues
Argentina · England · Spain · Brazil · UEFA · CONMEBOL

### Persistence
Full save/load via SQLite with custom serialization.

## What's still missing

- AI manager for rival clubs
- Training and finances
- Scouting system
- Some pages still in progress

## Run it

Requires .NET 10 SDK.

```bash
dotnet restore
dotnet run --project FootballEngine.Client
```

## License

MIT