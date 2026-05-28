namespace FootballEngine.Components

open Avalonia
open Avalonia.Controls
open Avalonia.Layout
open Avalonia.Media
open FootballEngine.Domain

module CommentaryOverlay =

    let commentaryForEvent (e: MatchEvent) (playerName: string) : string =
        match e.Type with
        | MatchEventType.Goal -> $"GOAL! {playerName} scores!"
        | MatchEventType.OwnGoal -> $"Oh no! {playerName} scores an own goal!"
        | MatchEventType.YellowCard -> $"Yellow card for {playerName}"
        | MatchEventType.RedCard -> $"Red card! {playerName} is sent off!"
        | MatchEventType.Save -> $"Great save!"
        | MatchEventType.SaveCaught _ -> $"Caught by the keeper!"
        | MatchEventType.SaveParried _ -> $"Parried away!"
        | MatchEventType.Corner -> $"Corner kick"
        | MatchEventType.FoulCommitted -> $"Foul!"
        | MatchEventType.ShotOnTarget -> $"Shot on target!"
        | MatchEventType.ShotOffTarget -> $"Shot off target"
        | MatchEventType.PassCompleted _ -> $"Nice pass"
        | _ -> ""

    let buildCommentary (text: string) : TextBlock =
        TextBlock(
            Text = text,
            FontSize = 16.0,
            FontWeight = FontWeight.Bold,
            Foreground = SolidColorBrush(Colors.White),
            HorizontalAlignment = HorizontalAlignment.Center,
            VerticalAlignment = VerticalAlignment.Bottom,
            Margin = Thickness(0.0, 0.0, 0.0, 10.0)
        )
