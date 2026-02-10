namespace FootballEngine

open FootballEngine.Domain

module FormationData =
    let private createSlot index role x y =
        { Index = index
          Role = role
          X = x
          Y = y }

    let rec getFormation formationName : FormationSlot list =
        match formationName with
        | "4-4-2" ->
            [ createSlot 0 "GK" 0.5 0.90
              createSlot 1 "LB" 0.1 0.75
              createSlot 2 "CB" 0.35 0.80
              createSlot 3 "CB" 0.65 0.80
              createSlot 4 "RB" 0.85 0.75
              createSlot 5 "LM" 0.1 0.45
              createSlot 6 "CM" 0.4 0.50
              createSlot 7 "CM" 0.6 0.50
              createSlot 8 "RM" 0.85 0.45
              createSlot 9 "ST" 0.35 0.15
              createSlot 10 "ST" 0.65 0.15 ]
        | "4-3-3" ->
            [ createSlot 0 "GK" 0.5 0.90
              createSlot 1 "LB" 0.1 0.75
              createSlot 2 "CB" 0.35 0.80
              createSlot 3 "CB" 0.65 0.80
              createSlot 4 "RB" 0.9 0.75
              createSlot 5 "CM" 0.3 0.50
              createSlot 6 "CDM" 0.5 0.60
              createSlot 7 "CM" 0.7 0.50
              createSlot 8 "LW" 0.15 0.20
              createSlot 9 "ST" 0.5 0.15
              createSlot 10 "RW" 0.85 0.20 ]
        | "3-5-2" ->
            [ createSlot 0 "GK" 0.5 0.90
              createSlot 1 "CB" 0.2 0.75
              createSlot 2 "CB" 0.5 0.75
              createSlot 3 "CB" 0.8 0.75
              createSlot 4 "LWB" 0.15 0.50
              createSlot 5 "CDM" 0.4 0.60
              createSlot 6 "CDM" 0.6 0.60
              createSlot 7 "RWB" 0.85 0.50
              createSlot 8 "CAM" 0.5 0.35
              createSlot 9 "ST" 0.35 0.15
              createSlot 10 "ST" 0.65 0.15 ]
        | _ -> getFormation "4-4-2"

    let availableFormations = [ "4-4-2"; "4-3-3"; "3-5-2" ]
