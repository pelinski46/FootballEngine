namespace FootballEngine.Generation

open System
open FootballEngine.Domain
open FootballEngine.Data
open FootballEngine.Stats

module StaffGen =

    let private coachingFor (role: StaffRole) =
        let high = normalInt 14.0 2.0 10 20
        let mid = normalInt 10.0 2.0 5 16
        let low = normalInt 6.0 2.0 1 10

        match role with
        | HeadCoach ->
            { Attacking = high
              Defending = high
              Fitness = mid
              Goalkeeping = mid
              Mental = high
              SetPieces = mid
              Tactical = high
              Technical = high
              WorkingWithYoungsters = mid }
        | AssistantManager ->
            { Attacking = high
              Defending = high
              Fitness = mid
              Goalkeeping = mid
              Mental = high
              SetPieces = mid
              Tactical = high
              Technical = mid
              WorkingWithYoungsters = mid }
        | FirstTeamCoach ->
            { Attacking = high
              Defending = high
              Fitness = mid
              Goalkeeping = low
              Mental = mid
              SetPieces = mid
              Tactical = mid
              Technical = high
              WorkingWithYoungsters = low }
        | GoalkeeperCoach ->
            { Attacking = low
              Defending = mid
              Fitness = mid
              Goalkeeping = high
              Mental = mid
              SetPieces = low
              Tactical = low
              Technical = mid
              WorkingWithYoungsters = low }
        | FitnessCoach ->
            { Attacking = low
              Defending = low
              Fitness = high
              Goalkeeping = low
              Mental = mid
              SetPieces = low
              Tactical = low
              Technical = low
              WorkingWithYoungsters = mid }
        | HeadOfYouthDevelopment ->
            { Attacking = mid
              Defending = mid
              Fitness = mid
              Goalkeeping = low
              Mental = mid
              SetPieces = low
              Tactical = mid
              Technical = mid
              WorkingWithYoungsters = high }
        | Scout ->
            { Attacking = low
              Defending = low
              Fitness = low
              Goalkeeping = low
              Mental = mid
              SetPieces = low
              Tactical = mid
              Technical = low
              WorkingWithYoungsters = mid }
        | Physio ->
            { Attacking = low
              Defending = low
              Fitness = high
              Goalkeeping = low
              Mental = mid
              SetPieces = low
              Tactical = low
              Technical = low
              WorkingWithYoungsters = low }
        | SportsScientist ->
            { Attacking = low
              Defending = low
              Fitness = high
              Goalkeeping = low
              Mental = mid
              SetPieces = low
              Tactical = low
              Technical = low
              WorkingWithYoungsters = low }
        | PerformanceAnalyst ->
            { Attacking = mid
              Defending = mid
              Fitness = low
              Goalkeeping = low
              Mental = mid
              SetPieces = mid
              Tactical = high
              Technical = low
              WorkingWithYoungsters = low }
        | RecruitmentAnalyst ->
            { Attacking = low
              Defending = low
              Fitness = low
              Goalkeeping = low
              Mental = mid
              SetPieces = low
              Tactical = mid
              Technical = low
              WorkingWithYoungsters = mid }
        | LoanManager ->
            { Attacking = low
              Defending = low
              Fitness = low
              Goalkeeping = low
              Mental = mid
              SetPieces = low
              Tactical = mid
              Technical = low
              WorkingWithYoungsters = high }

    let private scoutingFor (role: StaffRole) =
        let high = normalInt 14.0 2.0 10 20
        let mid = normalInt 10.0 2.0 5 16
        let low = normalInt 6.0 2.0 1 10

        match role with
        | Scout
        | RecruitmentAnalyst ->
            { JudgingAbility = high
              JudgingPotential = high
              Adaptability = high }
        | HeadCoach
        | AssistantManager ->
            { JudgingAbility = high
              JudgingPotential = high
              Adaptability = mid }
        | HeadOfYouthDevelopment
        | LoanManager ->
            { JudgingAbility = mid
              JudgingPotential = high
              Adaptability = mid }
        | PerformanceAnalyst ->
            { JudgingAbility = high
              JudgingPotential = mid
              Adaptability = mid }
        | FirstTeamCoach
        | GoalkeeperCoach
        | FitnessCoach
        | Physio
        | SportsScientist ->
            { JudgingAbility = low
              JudgingPotential = low
              Adaptability = low }

    let private medicalFor (role: StaffRole) =
        let high = normalInt 14.0 2.0 10 20
        let mid = normalInt 10.0 2.0 5 16
        let low = normalInt 6.0 2.0 1 10

        match role with
        | Physio ->
            { Physiotherapy = high
              SportsScience = mid }
        | SportsScientist ->
            { Physiotherapy = mid
              SportsScience = high }
        | FitnessCoach ->
            { Physiotherapy = mid
              SportsScience = mid }
        | HeadCoach
        | AssistantManager
        | FirstTeamCoach
        | GoalkeeperCoach
        | HeadOfYouthDevelopment
        | Scout
        | PerformanceAnalyst
        | RecruitmentAnalyst
        | LoanManager ->
            { Physiotherapy = low
              SportsScience = low }

    let private analysisFor (role: StaffRole) =
        let high = normalInt 14.0 2.0 10 20
        let mid = normalInt 10.0 2.0 5 16
        let low = normalInt 6.0 2.0 1 10

        match role with
        | PerformanceAnalyst ->
            { PerformanceAnalysis = high
              RecruitmentAnalysis = mid }
        | RecruitmentAnalyst ->
            { PerformanceAnalysis = mid
              RecruitmentAnalysis = high }
        | HeadCoach
        | AssistantManager ->
            { PerformanceAnalysis = mid
              RecruitmentAnalysis = mid }
        | FirstTeamCoach
        | GoalkeeperCoach
        | FitnessCoach
        | HeadOfYouthDevelopment
        | Scout
        | Physio
        | SportsScientist
        | LoanManager ->
            { PerformanceAnalysis = low
              RecruitmentAnalysis = low }

    let private mentalFor (role: StaffRole) =
        let high = normalInt 14.0 2.0 10 20
        let mid = normalInt 10.0 2.0 5 16
        let low = normalInt 6.0 2.0 1 10

        match role with
        | HeadCoach ->
            { Adaptability = mid
              Determination = high
              LevelOfDiscipline = high
              ManManagement = high
              Motivating = high
              PlayerKnowledge = high
              YoungsterKnowledge = mid }
        | AssistantManager ->
            { Adaptability = mid
              Determination = high
              LevelOfDiscipline = mid
              ManManagement = high
              Motivating = high
              PlayerKnowledge = high
              YoungsterKnowledge = mid }
        | FirstTeamCoach ->
            { Adaptability = mid
              Determination = high
              LevelOfDiscipline = mid
              ManManagement = mid
              Motivating = mid
              PlayerKnowledge = high
              YoungsterKnowledge = low }
        | GoalkeeperCoach ->
            { Adaptability = mid
              Determination = high
              LevelOfDiscipline = mid
              ManManagement = low
              Motivating = mid
              PlayerKnowledge = mid
              YoungsterKnowledge = low }
        | FitnessCoach ->
            { Adaptability = mid
              Determination = high
              LevelOfDiscipline = high
              ManManagement = low
              Motivating = mid
              PlayerKnowledge = low
              YoungsterKnowledge = low }
        | HeadOfYouthDevelopment ->
            { Adaptability = mid
              Determination = high
              LevelOfDiscipline = mid
              ManManagement = mid
              Motivating = mid
              PlayerKnowledge = high
              YoungsterKnowledge = high }
        | Scout ->
            { Adaptability = high
              Determination = high
              LevelOfDiscipline = low
              ManManagement = low
              Motivating = low
              PlayerKnowledge = high
              YoungsterKnowledge = high }
        | Physio
        | SportsScientist ->
            { Adaptability = mid
              Determination = high
              LevelOfDiscipline = mid
              ManManagement = low
              Motivating = low
              PlayerKnowledge = low
              YoungsterKnowledge = low }
        | PerformanceAnalyst ->
            { Adaptability = mid
              Determination = high
              LevelOfDiscipline = low
              ManManagement = low
              Motivating = low
              PlayerKnowledge = high
              YoungsterKnowledge = low }
        | RecruitmentAnalyst ->
            { Adaptability = high
              Determination = high
              LevelOfDiscipline = low
              ManManagement = low
              Motivating = low
              PlayerKnowledge = high
              YoungsterKnowledge = high }
        | LoanManager ->
            { Adaptability = high
              Determination = mid
              LevelOfDiscipline = low
              ManManagement = high
              Motivating = mid
              PlayerKnowledge = high
              YoungsterKnowledge = high }

    let private badgeForRole (role: StaffRole) (reputation: int) =
        match role with
        | HeadCoach
        | AssistantManager ->
            match reputation with
            | r when r >= 7000 -> ContinentalPro
            | r when r >= 5000 -> ProLicense
            | r when r >= 3000 -> NationalA
            | r when r >= 1500 -> NationalB
            | _ -> NationalC
        | FirstTeamCoach
        | GoalkeeperCoach
        | FitnessCoach
        | HeadOfYouthDevelopment -> if bernoulli 0.4 then NationalB else NationalC
        | Scout
        | Physio
        | SportsScientist
        | PerformanceAnalyst
        | RecruitmentAnalyst
        | LoanManager -> NoneBadge

    let private reputationFor (leagueLevel: int) (role: StaffRole) =
        match role with
        | HeadCoach ->
            match leagueLevel with
            | 0 -> normalInt 6000.0 1000.0 2000 9999
            | 1 -> normalInt 3500.0 800.0 500 6000
            | _ -> normalInt 1500.0 600.0 100 4000
        | AssistantManager ->
            match leagueLevel with
            | 0 -> normalInt 4000.0 800.0 1000 7000
            | _ -> normalInt 2000.0 600.0 200 5000
        | FirstTeamCoach
        | GoalkeeperCoach
        | FitnessCoach
        | HeadOfYouthDevelopment -> normalInt 1500.0 600.0 100 4000
        | Scout
        | RecruitmentAnalyst
        | LoanManager -> normalInt 1000.0 400.0 100 3000
        | Physio
        | SportsScientist
        | PerformanceAnalyst -> normalInt 800.0 300.0 100 2500

    let create
        (staffId: StaffId)
        (role: StaffRole)
        (clubId: ClubId)
        (countryData: CountryData)
        (leagueLevel: int)
        (year: int)
        : Staff =
        let reputation = reputationFor leagueLevel role
        let age = normalInt 40.0 7.0 28 65

        { Id = staffId
          Name =
            pickWeighted (countryData.Names.FirstNames |> List.map (fun name -> 1.0, name))
            + " "
            + pickWeighted (countryData.Names.LastNames |> List.map (fun name -> 1.0, name))
          Nationality = countryData.Country.Code
          Birthday = DateTime(year - age, normalInt 6.0 3.0 1 12, normalInt 15.0 8.0 1 28)
          Role = role
          Attributes =
            { Coaching = coachingFor role
              Scouting = scoutingFor role
              Medical = medicalFor role
              Analysis = analysisFor role }
          Mental = mentalFor role
          Badge = badgeForRole role reputation
          Reputation = reputation
          Contract =
            Some
                { ClubId = clubId
                  Salary = normalFloat 20_000.0 8_000.0 5_000.0 150_000.0 |> decimal
                  ExpiryYear = year + normalInt 2.0 1.0 1 4 }
          Status = Active
          TrophiesWon = 0
          SeasonsManaged = normalInt 5.0 4.0 0 20 }

    let rolesForClub =
        [ HeadCoach
          AssistantManager
          FirstTeamCoach
          GoalkeeperCoach
          FitnessCoach
          HeadOfYouthDevelopment
          Scout
          Scout
          Scout
          Physio
          SportsScientist
          PerformanceAnalyst
          RecruitmentAnalyst
          LoanManager ]
