<div align="center">

# ⚽ Football Engine 2026

**A deterministic, data-driven football management game built entirely in F#.**

[![.NET](https://img.shields.io/badge/.NET-10.0-512BD4)](#)
[![F#](https://img.shields.io/badge/F%23-10.1-378BBA)](#)
[![AvaloniaUI](https://img.shields.io/badge/Avalonia-11.3-purple)](#)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![itch.io](https://img.shields.io/badge/Available%20on-itch.io-FA5C5C)](https://itch.io)

**[Download on itch.io](#) · [Latest Release](#) · [Report a Bug](#) · [Request a Feature](#)**

</div>

---

## What is Football Engine?

Football Engine is a football management simulation where the match engine actually simulates football — not a dice roll. Every match runs a full 2D spatial simulation at 40Hz: players move, press, drift, and make cognitive decisions based on their attributes, your tactics, and the state of the game. The world around each match — transfers, development, finances, and season progression — runs on the same principles: deterministic, data-driven, no hand-waving.

It ships as a **single self-contained executable**. No installer, no internet connection, no launcher. Unzip and run.

---

## Features

### The Match Engine

Matches are not resolved with a formula. They are simulated tick by tick at 40Hz on a 105×68 metre pitch with real physics units.

**Ball physics** account for air drag, ground friction, ground restitution on bounce, and Magnus effect spin. A ball struck with sidespin curves. A ball rolling on the ground decelerates correctly.

**Player movement** is governed by steering behaviours evaluated every tick: Seek, Arrive, Pursuit, Separation, and shape-maintenance against a base position grid derived from your formation. Players run off the ball — deep runs, overlaps, underlaps, diagonal runs, third-man runs, false nine drops — triggered by possession, space detection, or tactical instruction.

**Cognitive processing** runs on a separate, lower-frequency cycle. Each player computes a visibility mask and a field-of-view cone, then evaluates passing targets, press triggers, and spatial coverage using that perceptual state, not omniscient knowledge.

**Tactical state evolves during the match.** Emergent feedback loops track pass success rates, press success rates, and flank success rates, and nudge compactness, pressing intensity, and wing-play preference in response. A team winning comfortably plays differently from the same team chasing the game in the 80th minute.

**Fatigue is real.** Condition drains during the match. A team on a losing run with heavy training load enters matches with degraded compactness, pressing intensity, tempo, and risk appetite — modelled explicitly as cascading emergent state penalties.

**Momentum** is tracked as a continuous value across the match and influences urgency multipliers on both sides.

**Set pieces** are handled by a dedicated agent: corners, free kicks, throw-ins, goal kicks, penalties. Offside is checked at pass moment via snapshot, not at resolution.

### Player Attributes

Players have 30+ attributes across four stat groups:

| Group | Attributes |
|---|---|
| Physical | Acceleration, Pace, Agility, Balance, Jumping Reach, Stamina, Strength |
| Technical | Finishing, Long Shots, Dribbling, Ball Control, Passing, Crossing, Tackling, Marking, Heading, Free Kick, Penalty |
| Mental | Aggression, Composure, Vision, Positioning, Bravery, Work Rate, Concentration, Leadership |
| Goalkeeping | Reflexes, Handling, Kicking, One-on-One, Aerial Reach |

Each player's attributes generate a **behavioural profile** — a continuous representation of how they naturally move and decide during simulation. The profile feeds directly into steering decisions, risk tolerance, pass selection, and pressing triggers. Attributes aren't just numbers on a screen; they change what the player does on the pitch.

### Player Development

Development is probabilistic, age-gated, and attribute-sensitive.

Players under 20 can gain up to 4 skill points per development cycle; this ceiling drops as they age, reaching 0 after 30. Weekly development probability is scaled by training focus, training intensity, the gap between current skill and potential, and the player's behavioural profile — a player with high creativity weight develops faster under Technical focus. Heavy training increases development rate but increases injury risk and damages morale. Fatigue and losing spirals compound: poor morale → worse match performance → worse outcomes → lower morale.

### Transfers and Negotiations

Transfers run as a finite state machine with four stages: offer → club response → contract offer → player response.

Club response evaluates offer fee against market value (quadratic in skill) and buyer reputation against seller reputation. Player response evaluates club reputation against their own, and offered salary against current salary. Both sides can reject independently. Market value and salary are derived from skill via consistent formulas used everywhere in the game.

### Season and Competition Structure

Competitions support domestic leagues with configurable points, tiebreakers (goal difference, goals scored, head-to-head, head-to-head goal difference), promotion slots (automatic and playoff), and relegation slots. Cup formats support straight knockout and group-then-knockout with configurable group rules. International cups support confederation slots and multiple qualification paths. Two-legged ties resolve via away goals, extra time and penalties, replay at neutral, or penalties only — configurable per competition.

### Staff

The coaching staff has distinct roles — Head Coach, Assistant Manager, First Team Coach, Goalkeeper Coach, Fitness Coach, Head of Youth Development, Scout, Physio, Sports Scientist, Performance Analyst, Recruitment Analyst, Loan Manager, Technical Director — each with their own attribute set and effective ability formula. Coach quality affects training, squad development, and tactical execution. Head Coach performance against board objectives affects reputation and job security.

### Board Objectives

The board assigns objectives: Survival, Mid-Table, Top Half, Top Four, Win League, Promotion, Domestic Cup, Continental Cup, Champions League equivalent. Meeting them builds coach reputation. Failing twice in a row costs you the job.

### Mod System

The entire database is decoupled from the executable. Nations, leagues, cup formats, tiebreaker rules, promotion and relegation slots, qualification paths, and name pools are defined in JSON files loaded at startup.

The mod system validates loaded data against a strict schema before accepting it. Errors surface with precise messages — invalid JSON, schema version mismatch, duplicate country code, duplicate competition name, missing dependency, path traversal attempt, oversized file.

A built-in mod editor lets you create, modify, or patch data directly from the game without touching the files manually.

---

## Architecture

Football Engine enforces a functional, immutable-by-default architecture with one deliberate exception: the match simulation hot path uses mutable `TeamFrame` arrays in Structure-of-Arrays layout for cache performance at 40Hz.

| Layer | Technology |
|---|---|
| Language | F# (.NET 10), Units of Measure for all physics quantities |
| UI | Avalonia FuncUI + Elmish (Model-View-Update, unidirectional data flow) |
| Rendering | SkiaSharp via `ICustomDrawOperation` for the live match pitch |
| Statistics | FSharp.Stats — Normal, Beta, and Poisson distributions for stochastic resolution |
| Persistence | SQLite (`sqlite-net-pcl`) — transactional save/load |
| Error Handling | `Result` types and `FsToolkit.ErrorHandling` throughout |

All state mutations during simulation go through the Elmish message dispatch. The simulation itself is deterministic: same seed, same input, same output.

### Project Structure

```
FootballEngine/
├── FootballEngine.Client/
│   ├── AppState/          # Elmish model, messages, update functions
│   ├── Database/          # SQLite entities, mappers, repository
│   ├── Domain/            # Core types: Player, Club, Competition, Transfer, Staff
│   ├── Engine/
│   │   ├── Match/         # Simulator, ball physics, player agents, referee, set pieces
│   │   └── World/         # Season phases, transfer logic, development, finance
│   ├── GameData/          # JSON mod loader, validator, registry, mod editor types
│   └── Views/             # Avalonia FuncUI pages and components
│
├── FootballEngine.Tests/  # Expecto test suite
│   ├── MatchEngineTests/  # Physics invariants, offside logic, determinism
│   └── WorldTests/        # State integrity and scheduler contracts
│
└── FootballEngine.Benchmarks/ # BenchmarkDotNet profiling
```

---

## Getting Started

**Prerequisites:** [.NET 10.0 SDK](https://dotnet.microsoft.com/download)

```bash
git clone https://github.com/your-org/FootballEngine.git
cd FootballEngine
dotnet restore
dotnet run --project FootballEngine.Client/FootballEngine.Client.fsproj -c Release
```

The release build produces a single self-contained executable. No runtime installation required on the target machine.

---

## Downloads

**Free builds** are released on [GitHub Releases](#) as tagged self-contained executables for Windows, Linux, and macOS.

**Pay-what-you-want** builds are available on [itch.io](#). The source code remains open under the GPL — you are always free to build from source.

---

## Testing

```bash
# Run the test suite
dotnet run --project FootballEngine.Tests/FootballEngine.Tests.fsproj

# Run performance benchmarks
dotnet run --project FootballEngine.Benchmarks/FootballEngine.Benchmarks.fsproj -c Release
```

The test suite covers physics invariants (ball trajectory, restitution), offside detection, match determinism (same seed → same scoreline, same events), and world state integrity (no orphaned player IDs, no budget underflow, valid standing computations).

---

## License

Football Engine is free software: you can redistribute it and/or modify it under the terms of the **GNU General Public License v3.0** as published by the Free Software Foundation.

See [LICENSE](LICENSE) for the full text.

The source code is open. Builds on itch.io are pay-what-you-want. The GPL guarantees that any distributed modified version must also be open source.