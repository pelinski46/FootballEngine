module FootballEngine.Tests.MatchEngineTests.PossessionTests

open Expecto
open FootballEngine
open FootballEngine.Domain
open Helpers

let possessionTests =
    testList
        "Possession"
        [
            testCase "buildState sets ball phase" <| fun () ->
                let home = [| makePlayer 1 MC 10 |]
                let away = [| makePlayer 2 MC 10 |]

                let ctx, s =
                    buildState
                        home
                        [| 52.5, 34.0 |]
                        away
                        [| 53.0, 34.0 |]
                        52.5
                        34.0
                        (Owned (HomeClub, 0))

                Expect.equal s.Ball.Possession (Owned (HomeClub, 0)) "phase should match"

            testCase "possessor field can be set and read" <| fun () ->
                let home = [| makePlayer 1 MC 10 |]
                let away = [| makePlayer 2 MC 10 |]

                let ctx, s =
                    buildState
                        home
                        [| 52.5, 34.0 |]
                        away
                        [| 53.0, 34.0 |]
                        52.5
                        34.0
                        Loose

                s.Ball <- { s.Ball with LastTouchBy = Some 1 }
                Expect.equal s.Ball.LastTouchBy (Some 1) "possessor should be Some 1"
        ]
        
        
