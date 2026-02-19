namespace FootballEngine

open FootballEngine.Domain

module FormationData =
    let private createSlot index pos x y =
        { Index = index
          Role = pos
          X = x
          Y = y }

    let rec getFormation formationName : FormationSlot list =
        match formationName with
        | "4-4-2" ->
            [ createSlot 0 GK 0.5 0.90
              createSlot 1 DL 0.1 0.75
              createSlot 2 DC 0.35 0.80
              createSlot 3 DC 0.65 0.80
              createSlot 4 DR 0.85 0.75
              createSlot 5 ML 0.1 0.45
              createSlot 6 MC 0.4 0.50
              createSlot 7 MC 0.6 0.50
              createSlot 8 MR 0.85 0.45
              createSlot 9 ST 0.35 0.15
              createSlot 10 ST 0.65 0.15 ]
        | "4-3-3" ->
            [ createSlot 0 GK 0.5 0.90
              createSlot 1 DL 0.1 0.75
              createSlot 2 DC 0.35 0.80
              createSlot 3 DC 0.65 0.80
              createSlot 4 DR 0.9 0.75
              createSlot 5 MC 0.3 0.50
              createSlot 6 DM 0.5 0.60 // Nota: "CDM" pasa a ser DM
              createSlot 7 MC 0.7 0.50
              createSlot 8 AML 0.15 0.20 // "LW" pasa a AML
              createSlot 9 ST 0.5 0.15
              createSlot 10 AMR 0.85 0.20 ] // "RW" pasa a AMR
        | "3-5-2" ->
            [ createSlot 0 GK 0.5 0.90
              createSlot 1 DC 0.2 0.75
              createSlot 2 DC 0.5 0.75
              createSlot 3 DC 0.8 0.75
              createSlot 4 WBL 0.15 0.50 // "LWB" pasa a WBL
              createSlot 5 DM 0.4 0.60 // "CDM" pasa a DM
              createSlot 6 DM 0.6 0.60
              createSlot 7 WBR 0.85 0.50 // "RWB" pasa a WBR
              createSlot 8 AMC 0.5 0.35 // "CAM" pasa a AMC
              createSlot 9 ST 0.35 0.15
              createSlot 10 ST 0.65 0.15 ]
        | _ -> getFormation "4-4-2"

    let availableFormations = [ "4-4-2"; "4-3-3"; "3-5-2" ]
