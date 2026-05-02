namespace FootballEngine.Movement

open FootballEngine.Domain
// ── Params split by system responsibility ───────────────────



module TeamDirectiveOps =

    let kindFromTactics (teamTactics: TeamTactics) : DirectiveKind =
        match teamTactics with
        | TeamTactics.Attacking -> DirectAttack
        | TeamTactics.Pressing -> PressingBlock
        | TeamTactics.Defensive -> DefensiveBlock
        | TeamTactics.Counter -> CounterReady
        | _ -> Structured

    let suspend (state: TeamDirectiveState) : TeamDirectiveState =
        match state with
        | TeamDirectiveState.Active d -> TeamDirectiveState.Suspended d
        | TeamDirectiveState.Transitioning(_, d, _) -> TeamDirectiveState.Suspended d
        | TeamDirectiveState.Suspended _ as s -> s

    let resume (state: TeamDirectiveState) : TeamDirectiveState =
        match state with
        | TeamDirectiveState.Suspended d -> TeamDirectiveState.Active d
        | other -> other

    let currentDirective (state: TeamDirectiveState) : TeamDirective option =
        match state with
        | TeamDirectiveState.Active d -> Some d
        | TeamDirectiveState.Transitioning(_, d, _) -> Some d
        | TeamDirectiveState.Suspended d -> Some d

    let empty (subTick: int) : TeamDirective =
        { Kind = Structured
          Params =
            { Press =
                { Intensity = 0.5
                  TriggerZone = MidfieldZone
                  MinPresserCount = 3 }
              Shape =
                { Width = 0.5
                  DefensiveLineHeight = 0.0
                  Compactness = 0.5 }
              Transition =
                { Tempo = 0.5
                  DirectnessThreshold = 0.5
                  CounterTrigger = false
                  WingBias       = 0.0
                  DirectnessBias = 0.0 } }
          TargetRunner = None
          RunType = None
          RunTarget = None
          ActiveSince = subTick }
