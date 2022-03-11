module Animation exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Simple.Animation as Animation exposing (Animation)
import Simple.Animation.Animated as Animated
import Simple.Animation.Property as P


fadeAndShrinkAway : Int -> List (Attribute msg) -> List (Html msg) -> Html msg
fadeAndShrinkAway index attributes msg =
    let
        delayInMillis =
            100 * index

        fadeAndShrink =
            Animation.fromTo
                { duration = 800, options = [ Animation.delay delayInMillis ] }
                [ P.rotate 0, P.opacity 1, P.scale 1 ]
                [ P.rotate 180, P.opacity 0, P.scale 0 ]
    in
    Animated.div fadeAndShrink attributes msg


pulse : List (Attribute msg) -> List (Html msg) -> Html msg
pulse attributes msg =
    let
        pulseLoop =
            Animation.steps
                { startAt = [ P.scale 0.5 ]
                , options = [ Animation.loop ]
                }
                [ Animation.step 100 [ P.scale 1 ]
                , Animation.wait 10
                , Animation.step 100 [ P.scale 0.5 ]
                ]
    in
    Animated.div pulseLoop attributes msg
