module Animation exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Simple.Animation as Animation exposing (Animation)
import Simple.Animation.Animated as Animated
import Simple.Animation.Property as P


fadeAway : Int -> List (Attribute msg) -> List (Html msg) -> Html msg
fadeAway index attributes msg =
    let
        delayInMillis =
            150 * index

        fade =
            Animation.fromTo
                { duration = 800, options = [ Animation.delay delayInMillis ] }
                [ P.rotate 0, P.opacity 1, P.scale 1 ]
                [ P.rotate 180, P.opacity 0, P.scale 0 ]
    in
    Animated.div fade attributes msg


pulse : List (Attribute msg) -> List (Html msg) -> Html msg
pulse attributes msg =
    let
        pulseLoop =
            Animation.fromTo
                { duration = 200, options = [ Animation.loop ] }
                [ P.scale 0.5 ]
                [ P.scale 1 ]
    in
    Animated.div pulseLoop attributes msg
