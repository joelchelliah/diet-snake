module Pill exposing (..)

import Types exposing (Pill, Position)


color : { green : String, blue : String, yellow : String, pink : String, teal : String }
color =
    -- Must have corresponding CSS classes
    { green = "green"
    , blue = "blue"
    , yellow = "yellow"
    , pink = "pink"
    , teal = "teal"
    }


allColors : List String
allColors =
    [ color.green, color.blue, color.yellow, color.pink, color.teal ]


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
