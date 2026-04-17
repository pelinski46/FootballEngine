module FootballEngine.Tests.MatchEngineTests.PossessionTests

open Expecto
open FootballEngine
open FootballEngine.Domain
open SimStateOps
open Helpers

let private mkSnapshot () =
    { PasserId = 1
      ReceiverId = 2
      ReceiverXAtPass = 80.0
      SecondLastDefenderX = 75.0
      BallXAtPass = 52.5
      Dir = LeftToRight }

let possessionPhaseTests =
    testList
        "Possession"
        [

          testCase "flipPossession from InPossession HomeClub → Contest AwayClub"
          <| fun () ->
              let _, s =
                  buildState
                      [| makePlayer 1 MC 10 |]
                      [| 52.5, 34.0 |]
                      [| makePlayer 2 MC 10 |]
                      [| 52.5, 34.0 |]
                      52.5
                      34.0
                      (Owned(HomeClub, 1))

              flipPossession s
              let expected = Contest AwayClub

              Expect.equal
                  s.Ball.Possession
                  expected
                  $"flipPossession(InPossession HomeClub): Phase = %A{s.Ball.Possession}, expected %A{expected}"

              Expect.isNone s.Ball.ControlledBy $"flipPossession: ControlledBy = %A{s.Ball.ControlledBy}, expected None"

          testCase "flipPossession from Transition AwayClub → Contest HomeClub"
          <| fun () ->
              let _, s =
                  buildState
                      [| makePlayer 1 MC 10 |]
                      [| 52.5, 34.0 |]
                      [| makePlayer 2 MC 10 |]
                      [| 52.5, 34.0 |]
                      52.5
                      34.0
                      (Transition AwayClub)

              flipPossession s
              let expected = Contest HomeClub

              Expect.equal
                  s.Ball.Possession
                  expected
                  $"flipPossession(Transition AwayClub): Phase = %A{s.Ball.Possession}, expected %A{expected}"

          testCase "flipPossession from SetPiece HomeClub → Contest AwayClub"
          <| fun () ->
              let _, s =
                  buildState
                      [| makePlayer 1 MC 10 |]
                      [| 52.5, 34.0 |]
                      [| makePlayer 2 MC 10 |]
                      [| 52.5, 34.0 |]
                      52.5
                      34.0
                      (SetPiece HomeClub)

              flipPossession s
              let expected = Contest AwayClub

              Expect.equal
                  s.Ball.Possession
                  expected
                  $"flipPossession(SetPiece HomeClub): Phase = %A{s.Ball.Possession}, expected %A{expected}"

          testCase "flipPossession from InFlight AwayClub → Contest HomeClub"
          <| fun () ->
              let _, s =
                  buildState
                      [| makePlayer 1 MC 10 |]
                      [| 52.5, 34.0 |]
                      [| makePlayer 2 MC 10 |]
                      [| 52.5, 34.0 |]
                      52.5
                      34.0
                      (InFlight AwayClub)

              flipPossession s
              let expected = Contest HomeClub

              Expect.equal
                  s.Ball.Possession
                  expected
                  $"flipPossession(InFlight AwayClub): Phase = %A{s.Ball.Possession}, expected %A{expected}"

          testCase "flipPossession clears PendingOffsideSnapshot"
          <| fun () ->
              let _, s =
                  buildState
                      [| makePlayer 1 MC 10 |]
                      [| 52.5, 34.0 |]
                      [| makePlayer 2 MC 10 |]
                      [| 52.5, 34.0 |]
                      52.5
                      34.0
                      (Owned(HomeClub, 1))

              s.Ball <-
                  { s.Ball with
                      PendingOffsideSnapshot = Some(mkSnapshot ()) }

              flipPossession s

              Expect.isNone
                  s.Ball.PendingOffsideSnapshot
                  $"flipPossession: PendingOffsideSnapshot = %A{s.Ball.PendingOffsideSnapshot}, expected None"

          testCase "losePossession from InPossession HomeClub → Contest HomeClub"
          <| fun () ->
              let _, s =
                  buildState
                      [| makePlayer 1 MC 10 |]
                      [| 52.5, 34.0 |]
                      [| makePlayer 2 MC 10 |]
                      [| 52.5, 34.0 |]
                      52.5
                      34.0
                      (Owned(HomeClub, 1))

              losePossession s
              let expected = Contest HomeClub

              Expect.equal
                  s.Ball.Possession
                  expected
                  $"losePossession(InPossession HomeClub): Phase = %A{s.Ball.Possession}, expected %A{expected}"

              Expect.isNone s.Ball.ControlledBy $"losePossession: ControlledBy = %A{s.Ball.ControlledBy}, expected None"

          testCase "losePossession from Transition AwayClub → Contest AwayClub"
          <| fun () ->
              let _, s =
                  buildState
                      [| makePlayer 1 MC 10 |]
                      [| 52.5, 34.0 |]
                      [| makePlayer 2 MC 10 |]
                      [| 52.5, 34.0 |]
                      52.5
                      34.0
                      (Transition AwayClub)

              losePossession s
              let expected = Contest AwayClub

              Expect.equal
                  s.Ball.Possession
                  expected
                  $"losePossession(Transition AwayClub): Phase = %A{s.Ball.Possession}, expected %A{expected}"

          testCase "awardGoal HomeClub → SetPiece AwayClub, HomeScore incremented"
          <| fun () ->
              let ctx, s =
                  buildState
                      [| makePlayer 1 ST 10 |]
                      [| 52.5, 34.0 |]
                      [| makePlayer 2 MC 10 |]
                      [| 52.5, 34.0 |]
                      52.5
                      34.0
                      (Owned(HomeClub, 1))

              s.HomeScore <- 0
              awardGoal HomeClub (Some 1) 0 ctx s
              Expect.equal s.HomeScore 1 $"awardGoal HomeClub: HomeScore = {s.HomeScore}, expected 1"
              let expected = SetPiece AwayClub
              Expect.equal s.Ball.Possession expected $"awardGoal HomeClub: Phase = %A{s.Ball.Possession}, expected %A{expected}"

              Expect.equal
                  s.Ball.Position.X
                  PhysicsContract.HalfwayLineX
                  $"awardGoal: ball X = {s.Ball.Position.X}, expected {PhysicsContract.HalfwayLineX}"

          testCase "awardGoal AwayClub → SetPiece HomeClub, AwayScore incremented"
          <| fun () ->
              let ctx, s =
                  buildState
                      [| makePlayer 1 MC 10 |]
                      [| 52.5, 34.0 |]
                      [| makePlayer 2 ST 10 |]
                      [| 52.5, 34.0 |]
                      52.5
                      34.0
                      (Owned(AwayClub, 1))

              s.AwayScore <- 0
              awardGoal AwayClub (Some 2) 0 ctx s
              Expect.equal s.AwayScore 1 $"awardGoal AwayClub: AwayScore = {s.AwayScore}, expected 1"
              let expected = SetPiece HomeClub
              Expect.equal s.Ball.Possession expected $"awardGoal AwayClub: Phase = %A{s.Ball.Possession}, expected %A{expected}"

          testCase "resetBallToCenter preserves Phase, resets position"
          <| fun () ->
              let _, s =
                  buildState
                      [| makePlayer 1 MC 10 |]
                      [| 52.5, 34.0 |]
                      [| makePlayer 2 MC 10 |]
                      [| 52.5, 34.0 |]
                      80.0
                      30.0
                      (Owned(AwayClub, 1))

              s.Ball <- { s.Ball with Possession = Owned(HomeClub, 1 }
              resetBallToCenter s

              Expect.equal
                  s.Ball.Position.X
                  PhysicsContract.HalfwayLineX
                  $"resetBallToCenter: ball X = {s.Ball.Position.X}, expected {PhysicsContract.HalfwayLineX}"

              Expect.equal
                  s.Ball.Position.Y
                  (PhysicsContract.PitchWidth / 2.0)
                  $"resetBallToCenter: ball Y = {s.Ball.Position.Y}, expected {PhysicsContract.PitchWidth / 2.0}"

              Expect.isNone
                  s.Ball.ControlledBy
                  $"resetBallToCenter: ControlledBy = %A{s.Ball.ControlledBy}, expected None"

              let expected = Owned AwayClub
              Expect.equal s.Ball.Possession expected $"resetBallToCenter: Phase = %A{s.Ball.Possession}, expected %A{expected}"

          testCase "clearOffsideSnapshot sets PendingOffsideSnapshot to None"
          <| fun () ->
              let _, s =
                  buildState
                      [| makePlayer 1 MC 10 |]
                      [| 52.5, 34.0 |]
                      [| makePlayer 2 MC 10 |]
                      [| 52.5, 34.0 |]
                      52.5
                      34.0
                      (InFlight HomeClub)

              s.Ball <-
                  { s.Ball with
                      PendingOffsideSnapshot = Some(mkSnapshot ()) }

              clearOffsideSnapshot s

              Expect.isNone
                  s.Ball.PendingOffsideSnapshot
                  $"clearOffsideSnapshot: PendingOffsideSnapshot = %A{s.Ball.PendingOffsideSnapshot}, expected None" ]
