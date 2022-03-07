module Animation exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Simple.Animation as Animation exposing (Animation)
import Simple.Animation.Animated as Animated
import Simple.Animation.Property as P


fadeAndShrinkAway : List (Attribute msg) -> List (Html msg) -> Html msg
fadeAndShrinkAway attributes msg =
    let
        fadeAndShrink =
            Animation.fromTo
                { duration = 4000, options = [] }
                [ P.opacity 1, P.scale 1 ]
                [ P.opacity 0, P.scale 0 ]
    in
    Animated.div fadeAndShrink attributes msg
