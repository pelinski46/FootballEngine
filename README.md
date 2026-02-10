# FootballEngine

A proof-of-concept football management simulation built with F# and Avalonia UI.

## About This Project

FootballEngine is my experimental journey into creating a football management simulator using functional programming. I'm building this as a learning project to explore both game development concepts and functional programming patterns with F#. The current version demonstrates core mechanics like player statistics, basic match simulation, and team management interfaces.

## What's Working So Far

- **Domain Models**: I've built comprehensive player and club data structures
- **Match Engine**: Basic match simulation 
- **UI Framework**: Using Avalonia and Func UI to create the interface with multiple views
- **Data Persistence**: SQLite integration for saving game state
- **Multi-League Structure**: Support for leagues from England, Argentina, Spain, and Brazil

## Tech Stack

- **F#**: Chosen for its functional programming approach to game logic
- **Avalonia UI**: Cross-platform user interface framework
- **Elmish**: Model-View-Update architecture pattern for state management
- **SQLite**: Local data storage solution

## Project Structure

```
FootballEngine/
├── FootballEngine.Client/
│   ├── Domain.fs          # Core domain models and types
│   ├── Database.fs        # Data access layer    
│   ├── Engine.fs          # Match simulation logic
│   ├── AppState.fs        # Application state management
│   ├── GameData.fs        # Static game data (teams, names, etc.)
│   ├── FormationData.fs   # Tactical formation definitions
│   ├── Program.fs         # Application entry point
│   └── Views/             # UI components
│       ├── Components/    # Reusable UI elements
│       └── Pages/         # Main application views
└── .gitignore
```

## Getting Started

### What You'll Need

- .NET 10.0 SDK
- IDE with F# support (Rider, Visual Studio, or VS Code)

### Installation Steps

1. Clone the repository
2. Navigate to the project directory
3. Restore dependencies:
   ```bash
   dotnet restore
   ```
4. Build the project:
   ```bash
   dotnet build
   ```
5. Run the application:
   ```bash
   dotnet run --project FootballEngine.Client
   ```

## Current Status

This is a proof-of-concept in early development. Many features are still being implemented:

- Done: Basic domain models and data structures
- Done: Basic UI and navigation
- Done: Match simulation engine
- In Progress: Team management interfaces
- In Progress: Tactical system implementation
- Not Started: Transfer market system
- Not Started: Advanced AI for opponent teams
- Not Started: Financial management system

## Contributing

As this is a learning proof-of-concept, contributions are focused on core mechanics and architecture improvements. I welcome feedback, bug reports, and suggestions for fundamental enhancements.

## License

This project is licensed under the MIT License.
