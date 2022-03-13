module Animation exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Simple.Animation as Animation
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
                [ Animation.step 500 [ P.scale 1.15, P.rotate 15 ]
                , Animation.step 250 [ P.scale 0.5, P.rotate 0 ]
                , Animation.step 500 [ P.scale 1.15, P.rotate -15 ]
                , Animation.step 250 [ P.scale 0.5, P.rotate 0 ]
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
            round (450 * degrees / 360)

        grow =
            Animation.fromTo
                { duration = duration, options = [ Animation.easeOutQuint ] }
                [ P.rotate windup, P.opacity 0, P.scale 0 ]
                [ P.rotate 0, P.opacity 1, P.scale 1 ]
    in
    Animated.div grow


flipAround : Bool -> List (Attribute msg) -> List (Html msg) -> Html msg
flipAround upsideDown =
    let
        flip =
            Animation.fromTo
                { duration = 800, options = [] }
                [ if upsideDown then
                    P.rotate 0

                  else
                    P.rotate 1
                ]
                [ if upsideDown then
                    P.rotate 1

                  else
                    P.rotate 0
                ]
    in
    Animated.div flip
