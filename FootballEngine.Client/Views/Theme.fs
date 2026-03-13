namespace FootballEngine

module Theme =

    let BgMain = "#0f172a"
    let BgSidebar = "#020617"
    let BgCard = "#1e293b"
    let BgHover = "#334155"

    let Border = "#334155"

    let TextMain = "#f1f5f9"
    let TextMuted = "#64748b"
    let TextSub = "#94a3b8"

    let Accent = "#10b981"
    let AccentAlt = "#3b82f6"
    let AccentLight = "#10b98133"

    let Success = "#10b981"
    let Warning = "#f59e0b"
    let Danger = "#ef4444"

    let DragBorder = "#3b82f6"
    let DragOverlay = "#3b82f622"

    let moraleColor morale =
        if morale > 70 then Success
        elif morale > 40 then Warning
        else Danger
