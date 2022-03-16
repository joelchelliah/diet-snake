module Components.Stats exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Types exposing (Msg, StatDetails, Stats)
import Utils.Animation exposing (fadeAndRise)
import Utils.Icon exposing (viewArrowUpIcon)


updateBest : Stats -> Stats
updateBest ({ current, best } as stats) =
    { stats
        | best =
            { best
                | weightLoss = max current.weightLoss best.weightLoss
                , stepsTaken = max current.stepsTaken best.stepsTaken
                , pillsTaken = max current.pillsTaken best.pillsTaken
            }
    }


updateCurrent : Bool -> Int -> Stats -> Stats
updateCurrent onPill weightLoss ({ current } as stats) =
    let
        pillsTaken =
            if onPill then
                1

            else
                0

        newCurrent =
            { current
                | stepsTaken = current.stepsTaken + 1
                , pillsTaken = current.pillsTaken + pillsTaken
                , weightLoss = current.weightLoss + weightLoss
            }
    in
    { stats | current = newCurrent }


view : Stats -> Html Msg
view { current, best, prevBest } =
    div [ class "stats-overview" ]
        [ div [ class "stats" ]
            [ viewStat "Distance covered" current.stepsTaken "cm" best.stepsTaken False
            , viewStat "Pills taken" current.pillsTaken "mg" best.pillsTaken False
            , viewStat "Weight lost" current.weightLoss "kg" best.weightLoss False
            ]
        , div [ class "stats-divider" ] []
        , div [ class "stats" ]
            [ viewStat "Longest distance" best.stepsTaken "cm " prevBest.stepsTaken True
            , viewStat "Most pills taken" best.pillsTaken "mg" prevBest.pillsTaken True
            , viewStat "Maximum weightloss" best.weightLoss "kg" prevBest.weightLoss True
            ]
        ]


viewStat : String -> Int -> String -> Int -> Bool -> Html msg
viewStat label currentValue postfix comparedValue showArrowOnLeft =
    let
        arrow =
            if currentValue > comparedValue then
                fadeAndRise [ class "arrow-up highlight" ] [ viewArrowUpIcon ]

            else
                div [ class "arrow-up" ] []

        score =
            div
                [ class
                    (if currentValue > comparedValue then
                        "stat highlight"

                     else
                        "stat"
                    )
                ]
                [ div [] [ text (label ++ ":") ]
                , div [] [ text (String.fromInt currentValue ++ " " ++ postfix) ]
                ]
    in
    div [ class "stat-and-arrow" ]
        (if showArrowOnLeft then
            [ arrow, score ]

         else
            [ score, arrow ]
        )


initDetails : StatDetails
initDetails =
    { weightLoss = 0
    , pillsTaken = 0
    , stepsTaken = 0
    }


init : StatDetails -> Stats
init best =
    { current =
        { weightLoss = 0
        , pillsTaken = 0
        , stepsTaken = 0
        }
    , best = best
    , prevBest = best
    }
