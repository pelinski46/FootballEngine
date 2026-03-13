namespace FootballEngine.DomainTypes

type PlayerId = int
type ClubId = int
type MatchId = int
type LeagueId = int
type CountryCode = string

type Position =
    | GK
    | DR
    | DC
    | DL
    | WBR
    | WBL
    | DM
    | MR
    | MC
    | ML
    | AMR
    | AMC
    | AML
    | ST



type Formation =

    | F442
    | F442Diamond
    | F433
    | F433Flat
    | F451
    | F4141
    | F4231
    | F4312
    | F4321

    | F352
    | F343
    | F3421

    | F532
    | F541
    | F523
