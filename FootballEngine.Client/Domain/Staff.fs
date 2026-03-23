namespace FootballEngine.Domain

open System

type StaffId = int

type CoachingBadge =
    | NoneBadge
    | NationalC
    | NationalB
    | NationalA
    | ProLicense
    | ContinentalPro

type StaffRole =
    | HeadCoach
    | AssistantManager
    | FirstTeamCoach
    | GoalkeeperCoach
    | FitnessCoach
    | HeadOfYouthDevelopment
    | Scout
    | Physio
    | SportsScientist
    | PerformanceAnalyst
    | RecruitmentAnalyst
    | LoanManager

type CoachingAttributes =
    { Attacking: int
      Defending: int
      Fitness: int
      Goalkeeping: int
      Mental: int
      SetPieces: int
      Tactical: int
      Technical: int
      WorkingWithYoungsters: int }

type ScoutingAttributes =
    { JudgingAbility: int
      JudgingPotential: int
      Adaptability: int }

type MedicalAttributes =
    { Physiotherapy: int
      SportsScience: int }

type AnalysisAttributes =
    { PerformanceAnalysis: int
      RecruitmentAnalysis: int }

type StaffAttributes =
    { Coaching: CoachingAttributes
      Scouting: ScoutingAttributes
      Medical: MedicalAttributes
      Analysis: AnalysisAttributes }

type StaffMentalAttributes =
    { Adaptability: int
      Determination: int
      LevelOfDiscipline: int
      ManManagement: int
      Motivating: int
      PlayerKnowledge: int
      YoungsterKnowledge: int }

type StaffStatus =
    | Active
    | UnderPressure
    | Sacked
    | Resigned
    | ContractExpired
    | Unemployed

type StaffContract =
    { ClubId: ClubId
      Salary: decimal
      ExpiryYear: int }

type Staff =
    { Id: StaffId
      Name: string
      Nationality: CountryCode
      Birthday: DateTime
      Role: StaffRole
      Attributes: StaffAttributes
      Mental: StaffMentalAttributes
      Badge: CoachingBadge
      Reputation: int
      Contract: StaffContract option
      Status: StaffStatus
      TrophiesWon: int
      SeasonsManaged: int }

module Staff =

    let age (currentDate: DateTime) (s: Staff) =
        let years = currentDate.Year - s.Birthday.Year

        if currentDate < s.Birthday.AddYears(years) then
            years - 1
        else
            years

    let clubId (s: Staff) = s.Contract |> Option.map _.ClubId

    let isEmployed (s: Staff) =
        s.Status = Active || s.Status = UnderPressure

    let isCoachingRole (s: Staff) =
        match s.Role with
        | HeadCoach
        | AssistantManager
        | FirstTeamCoach
        | GoalkeeperCoach
        | FitnessCoach
        | HeadOfYouthDevelopment -> true
        | Scout
        | Physio
        | SportsScientist
        | PerformanceAnalyst
        | RecruitmentAnalyst
        | LoanManager -> false

    let effectiveAbility (s: Staff) =
        let c = s.Attributes.Coaching
        let sc = s.Attributes.Scouting
        let md = s.Attributes.Medical
        let an = s.Attributes.Analysis
        let mn = s.Mental

        match s.Role with
        | HeadCoach ->
            let coachScore =
                [ c.Attacking; c.Defending; c.Tactical; c.Technical; c.Mental; c.SetPieces ]
                |> List.averageBy float

            let mentalScore =
                [ mn.ManManagement; mn.Motivating; mn.Determination; mn.LevelOfDiscipline ]
                |> List.averageBy float

            int ((coachScore + mentalScore) / 2.0)

        | AssistantManager ->
            let coachScore =
                [ c.Attacking; c.Defending; c.Tactical; c.Technical ] |> List.averageBy float

            let mentalScore =
                [ mn.ManManagement; mn.Motivating; mn.PlayerKnowledge ] |> List.averageBy float

            int ((coachScore + mentalScore) / 2.0)

        | FirstTeamCoach ->
            [ c.Attacking; c.Defending; c.Technical; c.Mental; c.Tactical ]
            |> List.averageBy float
            |> int

        | GoalkeeperCoach -> [ c.Goalkeeping; c.Technical; c.Mental ] |> List.averageBy float |> int

        | FitnessCoach -> [ c.Fitness; mn.Determination; md.SportsScience ] |> List.averageBy float |> int

        | HeadOfYouthDevelopment ->
            [ c.WorkingWithYoungsters; mn.YoungsterKnowledge; sc.JudgingPotential ]
            |> List.averageBy float
            |> int

        | Scout ->
            [ sc.JudgingAbility; sc.JudgingPotential; mn.Determination; sc.Adaptability ]
            |> List.averageBy float
            |> int

        | Physio ->
            [ md.Physiotherapy; mn.Determination; c.WorkingWithYoungsters ]
            |> List.averageBy float
            |> int

        | SportsScientist -> [ md.SportsScience; c.Fitness; mn.Determination ] |> List.averageBy float |> int

        | PerformanceAnalyst ->
            [ an.PerformanceAnalysis; mn.PlayerKnowledge; mn.Determination ]
            |> List.averageBy float
            |> int

        | RecruitmentAnalyst ->
            [ an.RecruitmentAnalysis; sc.JudgingAbility; sc.JudgingPotential ]
            |> List.averageBy float
            |> int

        | LoanManager ->
            [ mn.PlayerKnowledge; sc.JudgingPotential; mn.ManManagement ]
            |> List.averageBy float
            |> int

    let canBecomeHeadCoach (s: Staff) =
        match s.Role with
        | AssistantManager
        | FirstTeamCoach -> s.Badge >= NationalA
        | _ -> false

    let promoteToHeadCoach (s: Staff) : Staff = { s with Role = HeadCoach }

    let applySeasonOutcome (objective: BoardObjective) (achieved: bool) (s: Staff) : Staff =
        match s.Role with
        | HeadCoach ->
            let repDelta =
                match objective, achieved with
                | LeagueObjective WinLeague, true -> 500
                | LeagueObjective TopFour, true -> 300
                | LeagueObjective TopHalf, true -> 100
                | LeagueObjective MidTable, true -> 50
                | LeagueObjective Survival, true -> 30
                | Promotion, true -> 400
                | CupObjective WinChampionsLeague, true -> 600
                | CupObjective WinContinentalCup, true -> 400
                | CupObjective WinDomesticCup, true -> 200
                | Relegation, true -> 0
                | LeagueObjective WinLeague, false -> -300
                | LeagueObjective TopFour, false -> -200
                | LeagueObjective TopHalf, false -> -80
                | LeagueObjective MidTable, false -> -40
                | LeagueObjective Survival, false -> -200
                | Promotion, false -> -50
                | CupObjective _, false -> 0
                | Relegation, false -> -100

            let newStatus =
                match s.Status, achieved with
                | Active, false -> UnderPressure
                | UnderPressure, false -> Sacked
                | UnderPressure, true -> Active
                | other, _ -> other

            { s with
                Reputation = max 0 (min 10000 (s.Reputation + repDelta))
                Status = newStatus
                SeasonsManaged = s.SeasonsManaged + 1
                TrophiesWon = if achieved then s.TrophiesWon + 1 else s.TrophiesWon }

        | _ ->
            { s with
                SeasonsManaged = s.SeasonsManaged + 1 }
