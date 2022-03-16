module Components.Pill exposing (..)

import Types exposing (Pill, PillColor(..), Position)


colors : List PillColor
colors =
    -- Must have corresponding CSS classes
    [ Green, Blue, Yellow, Pink, Teal ]


toString : PillColor -> String
toString color =
    case color of
        Green ->
            "green"

        Blue ->
            "blue"

        Yellow ->
            "yellow"

        Pink ->
            "pink"

        Teal ->
            "teal"


rotation : { min : number, max : number, multiplier : number }
rotation =
    { min = 1
    , max = 8
    , multiplier = 10
    }


isHere : Position -> Maybe Pill -> Bool
isHere pos pill =
    case pill of
        Nothing ->
            False

        Just { position } ->
            position == pos


getAllColorsExceptPillColor : Pill -> List PillColor
getAllColorsExceptPillColor pill =
    List.filter (\col -> col /= pill.color) colors
