module Animation exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Simple.Animation as Animation exposing (Animation)
import Simple.Animation.Animated as Animated
import Simple.Animation.Property as P


fadeAway : Int -> List (Attribute msg) -> List (Html msg) -> Html msg
fadeAway index =
    let
        delayInMillis =
            100 * index

        fade =
            Animation.fromTo
                { duration = 800, options = [ Animation.delay delayInMillis ] }
                [ P.rotate 0, P.opacity 1, P.scale 1 ]
                [ P.rotate 180, P.opacity 0, P.scale 0 ]
    in
    Animated.div fade


pulse : List (Attribute msg) -> List (Html msg) -> Html msg
pulse =
    let
        pulseSteps =
            Animation.steps
                { startAt = [ P.scale 0.5, P.rotate 0 ], options = [ Animation.loop ] }
                [ Animation.step 500 [ P.scale 1, P.rotate 15 ]
                , Animation.step 300 [ P.scale 0.5, P.rotate 0 ]
                , Animation.step 500 [ P.scale 1, P.rotate -15 ]
                , Animation.step 300 [ P.scale 0.5, P.rotate 0 ]
                ]
    in
    Animated.div pulseSteps


growAppear : Float -> List (Attribute msg) -> List (Html msg) -> Html msg
growAppear spin =
    let
        degrees =
            spin * 180

        windup =
            0 - degrees

        duration =
            round (500 * degrees / 360)

        grow =
            Animation.fromTo
                { duration = duration, options = [ Animation.easeOutCubic ] }
                [ P.rotate windup, P.opacity 0, P.scale 0 ]
                [ P.rotate 0, P.opacity 1, P.scale 1 ]
    in
    Animated.div grow
