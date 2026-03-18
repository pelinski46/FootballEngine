namespace FootballEngine.Database

open FootballEngine.Domain

module Serializers =

    let parsePosition (s: string) =
        match s.Trim().ToUpper() with
        | "GK" -> GK
        | "DL" -> DL
        | "DC" -> DC
        | "DR" -> DR
        | "WBL" -> WBL
        | "WBR" -> WBR
        | "DM" -> DM
        | "ML" -> ML
        | "MC" -> MC
        | "MR" -> MR
        | "AML" -> AML
        | "AMC" -> AMC
        | "AMR" -> AMR
        | "ST" -> ST
        | _ -> MC

    let parseFoot (s: string) =
        match s.Trim().ToUpper() with
        | "LEFT" -> Left
        | "RIGHT" -> Right
        | "BOTH" -> Both
        | _ -> Right

    let formationToString =
        function
        | F442 -> "4-4-2"
        | F442Diamond -> "4-4-2 Diamond"
        | F433 -> "4-3-3"
        | F433Flat -> "4-3-3 Flat"
        | F451 -> "4-5-1"
        | F4141 -> "4-1-4-1"
        | F4231 -> "4-2-3-1"
        | F4312 -> "4-3-1-2"
        | F4321 -> "4-3-2-1"
        | F352 -> "3-5-2"
        | F343 -> "3-4-3"
        | F3421 -> "3-4-2-1"
        | F532 -> "5-3-2"
        | F541 -> "5-4-1"
        | F523 -> "5-2-3"

    let parseFormation =
        function
        | "4-4-2" -> F442
        | "4-4-2 Diamond" -> F442Diamond
        | "4-3-3" -> F433
        | "4-3-3 Flat" -> F433Flat
        | "4-5-1" -> F451
        | "4-1-4-1" -> F4141
        | "4-2-3-1" -> F4231
        | "4-3-1-2" -> F4312
        | "4-3-2-1" -> F4321
        | "3-5-2" -> F352
        | "3-4-3" -> F343
        | "3-4-2-1" -> F3421
        | "5-3-2" -> F532
        | "5-4-1" -> F541
        | "5-2-3" -> F523
        | _ -> F442

    let roundToString =
        function
        | GroupStage g -> $"GroupStage:{g}"
        | KnockoutRound teams -> $"KnockoutRound:{teams}"
        | ThirdPlace -> "ThirdPlace"
        | Final -> "Final"

    let parseRound (s: string) : Round =
        if s.StartsWith("GroupStage:") then
            GroupStage(int (s.Substring(11)))
        elif s.StartsWith("KnockoutRound:") then
            KnockoutRound(int (s.Substring(14)))
        else
            match s with
            | "ThirdPlace" -> ThirdPlace
            | _ -> Final

    let confederationToString =
        function
        | UEFA -> "UEFA"
        | CONMEBOL -> "CONMEBOL"
        | CONCACAF -> "CONCACAF"
        | CAF -> "CAF"
        | AFC -> "AFC"
        | OFC -> "OFC"

    let parseConfederation =
        function
        | "CONMEBOL" -> CONMEBOL
        | "CONCACAF" -> CONCACAF
        | "CAF" -> CAF
        | "AFC" -> AFC
        | "OFC" -> OFC
        | _ -> UEFA

    let private tieResolutionToString =
        function
        | ReplayAtNeutral -> "ReplayAtNeutral"
        | ExtraTimeThenPenalties -> "ExtraTimeThenPenalties"
        | AwayGoals -> "AwayGoals"
        | PenaltiesOnly -> "PenaltiesOnly"

    let private parseTieResolution =
        function
        | "ReplayAtNeutral" -> ReplayAtNeutral
        | "ExtraTimeThenPenalties" -> ExtraTimeThenPenalties
        | "AwayGoals" -> AwayGoals
        | _ -> PenaltiesOnly

    let private legFormatToString =
        function
        | SingleLeg r -> $"SingleLeg:{tieResolutionToString r}"
        | TwoLegs r -> $"TwoLegs:{tieResolutionToString r}"

    let private parseLegFormat (s: string) : LegFormat =
        if s.StartsWith("SingleLeg:") then
            SingleLeg(parseTieResolution (s.Substring(10)))
        else
            TwoLegs(parseTieResolution (s.Substring(8)))

    let private tiebreakerToString =
        function
        | GoalDifference -> "GD"
        | GoalsScored -> "GS"
        | HeadToHead -> "H2H"
        | HeadToHeadGoalDifference -> "H2HGD"

    let private parseTiebreaker =
        function
        | "GD" -> GoalDifference
        | "GS" -> GoalsScored
        | "H2H" -> HeadToHead
        | _ -> HeadToHeadGoalDifference

    let private parseList (sep: char) f (s: string) =
        if s = "" then
            []
        else
            s.Split(sep) |> Array.map f |> List.ofArray

    let private leagueRulesToString (r: LeagueRules) =
        let tbs = r.Tiebreakers |> List.map tiebreakerToString |> String.concat ","

        let pros =
            r.Promotion
            |> List.map (function
                | AutomaticPromotion n -> $"AP:{n}"
                | PlayoffPromotion n -> $"PP:{n}")
            |> String.concat ","

        let rels =
            r.Relegation
            |> List.map (function
                | AutomaticRelegation n -> $"AR:{n}"
                | PlayoffRelegation n -> $"PR:{n}")
            |> String.concat ","

        $"{r.PointsForWin}|{r.PointsForDraw}|{tbs}|{pros}|{rels}"

    let private parseLeagueRules (s: string) : LeagueRules =
        let parts = s.Split('|')

        let parsePromotion (x: string) =
            let p = x.Split(':')

            if p[0] = "AP" then
                AutomaticPromotion(int p[1])
            else
                PlayoffPromotion(int p[1])

        let parseRelegation (x: string) =
            let p = x.Split(':')

            if p[0] = "AR" then
                AutomaticRelegation(int p[1])
            else
                PlayoffRelegation(int p[1])

        { PointsForWin = int parts[0]
          PointsForDraw = int parts[1]
          Tiebreakers = parseList ',' parseTiebreaker parts[2]
          Promotion = parseList ',' parsePromotion parts[3]
          Relegation = parseList ',' parseRelegation parts[4] }

    let private groupRulesToString (g: GroupRules) =
        let tbs = g.Tiebreakers |> List.map tiebreakerToString |> String.concat ","
        $"{g.GroupCount};{g.TeamsPerGroup};{g.QualifyPerGroup};{g.PointsForWin};{g.PointsForDraw};{tbs}"

    let private parseGroupRules (s: string) : GroupRules =
        let parts = s.Split(';')

        { GroupCount = int parts[0]
          TeamsPerGroup = int parts[1]
          QualifyPerGroup = int parts[2]
          PointsForWin = int parts[3]
          PointsForDraw = int parts[4]
          Tiebreakers = parseList ',' parseTiebreaker parts[5] }

    let private cupFormatToString =
        function
        | StraightKnockout leg -> $"SK:{legFormatToString leg}"
        | GroupThenKnockout(grp, leg) -> $"GTK:{groupRulesToString grp}~{legFormatToString leg}"

    let private parseCupFormat (s: string) : CupFormat =
        if s.StartsWith("GTK:") then
            let inner = s.Substring(4)
            let idx = inner.IndexOf('~')
            GroupThenKnockout(parseGroupRules inner[.. idx - 1], parseLegFormat inner[idx + 1 ..])
        else
            StraightKnockout(parseLegFormat (s.Substring(3)))

    let private qualificationSlotToString =
        function
        | LeaguePosition(LeagueLevel lvl, from, too) -> $"LP:{lvl}:{from}:{too}"
        | CupWinner name -> $"CW:{name}"
        | TitleHolder -> "TH"
        | ConfederationSlot n -> $"CS:{n}"

    let private parseQualificationSlot (s: string) : QualificationSlot =
        if s.StartsWith("LP:") then
            let p = s.Substring(3).Split(':')
            LeaguePosition(LeagueLevel(int p[0]), int p[1], int p[2])
        elif s.StartsWith("CW:") then
            CupWinner(s.Substring(3))
        elif s = "TH" then
            TitleHolder
        else
            ConfederationSlot(int (s.Substring(3)))

    let competitionTypeToStrings (ct: CompetitionType) : string * string * string =
        match ct with
        | NationalLeague(LeagueLevel lvl, rules) -> "NationalLeague", $"{lvl}", leagueRulesToString rules
        | NationalCup(fmt, slots) ->
            let slotStr = slots |> List.map qualificationSlotToString |> String.concat ","
            "NationalCup", cupFormatToString fmt, slotStr
        | InternationalCup(confOpt, fmt, slots) ->
            let confStr = confOpt |> Option.map confederationToString |> Option.defaultValue ""
            let slotStr = slots |> List.map qualificationSlotToString |> String.concat ","
            "InternationalCup", $"{cupFormatToString fmt}~{confStr}", slotStr

    let parseCompetitionType (tag: string) (p1: string) (p2: string) : CompetitionType =
        let defaultLeague =
            NationalLeague(
                LeagueLevel 0,
                { PointsForWin = 3
                  PointsForDraw = 1
                  Tiebreakers = [ GoalDifference ]
                  Promotion = []
                  Relegation = [] }
            )

        match tag with
        | "NationalLeague" -> NationalLeague(LeagueLevel(int p1), parseLeagueRules p2)
        | "NationalCup" -> NationalCup(parseCupFormat p1, parseList ',' parseQualificationSlot p2)
        | "InternationalCup" ->
            let idx = p1.LastIndexOf('~')

            let conf =
                let s = p1[idx + 1 ..] in if s = "" then None else Some(parseConfederation s)

            InternationalCup(conf, parseCupFormat p1[.. idx - 1], parseList ',' parseQualificationSlot p2)
        | _ -> defaultLeague
