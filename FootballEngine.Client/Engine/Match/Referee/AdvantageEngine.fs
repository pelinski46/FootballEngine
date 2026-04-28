namespace FootballEngine

open FootballEngine.Domain
open PhysicsContract
open Stats

type AdvantageState =
    | NoAdvantage
    | AdvantagePlaying of foulSubTick: int * fouledTeam: ClubSide * reason: string
    | AdvantageCalledBack of foulSubTick: int

type AdvantageDecision =
    | PlayOn of reason: string
    | StopPlay of reason: string

module AdvantageEngine =

    let advantageWindowSubTicks (clock: SimulationClock) : int =
        normalInt (4.0 * float clock.SubTicksPerSecond) 0.5 (3 * clock.SubTicksPerSecond) (5 * clock.SubTicksPerSecond)

    let evaluate
        (foulX: float<meter>)
        (foulY: float<meter>)
        (dir: AttackDir)
        (fouledTeam: ClubSide)
        (ballPossession: Possession)
        (zone: PitchZone)
        : AdvantageDecision =

        let hasPossession =
            match ballPossession with
            | Owned(side, _) -> side = fouledTeam
            | Contest(side) -> side = fouledTeam
            | Transition(side) -> side = fouledTeam
            | _ -> false

        if not hasPossession then
            StopPlay "no possession"
        else
            let effectiveX =
                match dir with
                | LeftToRight -> foulX
                | RightToLeft -> PitchLength - foulX

            let inDefensiveThird = effectiveX < 30.0<meter>
            let inAttackingThird = effectiveX > 75.0<meter>

            if inDefensiveThird then
                StopPlay "defensive zone"
            elif inAttackingThird && (zone = AttackingZone) then
                PlayOn "promising attack"
            elif zone = MidfieldZone then
                PlayOn "midfield advantage"
            else
                StopPlay "no advantage zone"

    let shouldCallBack
        (advSubTick: int)
        (currentSubTick: int)
        (ballPossession: Possession)
        (fouledTeam: ClubSide)
        : bool =

        let window = 5 * 40
        if currentSubTick - advSubTick > window then
            true
        else
            match ballPossession with
            | Owned(side, _) -> side <> fouledTeam
            | Contest(side) -> side <> fouledTeam
            | Transition(side) -> side <> fouledTeam
            | _ -> false
