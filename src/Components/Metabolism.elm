module Components.Metabolism exposing (..)

import Components.Pill exposing (rotation)
import Html exposing (progress)
import Model exposing (BulgeRotation(..), Metabolism)


maxBuglgeLength : number
maxBuglgeLength =
    -- Bulge size. Needs corresponding css classes (bulge-n).
    4


digestLength : number
digestLength =
    -- Number of tail tiles to visualise on
    20


initialRate : number
initialRate =
    -- Number of tail tiles to step through at a time
    2


flipRotation : BulgeRotation -> BulgeRotation
flipRotation rotation =
    if rotation == Clockwise then
        CounterClockwise

    else
        Clockwise


getRotationForIndex : Int -> Metabolism -> BulgeRotation
getRotationForIndex index { bulgeRotation } =
    -- To alternate rotation on each bulged tile
    if modBy 2 index == 0 then
        bulgeRotation

    else
        flipRotation bulgeRotation


digest : Bool -> Metabolism -> Metabolism
digest onPill ({ progress, bulgeRotation, isActive, rate } as metabolism) =
    -- Progress visualisation of the pill (bulge)
    -- moving through the tail while digesting the pill
    if onPill then
        { metabolism | progress = 0, rate = initialRate, isActive = True }

    else if progress == digestLength then
        { metabolism | isActive = False }

    else if isActive then
        { metabolism
            | progress = progress + rate
            , bulgeRotation = flipRotation bulgeRotation
        }

    else
        metabolism


getBulgeLength : Metabolism -> Int
getBulgeLength { progress } =
    -- To slowly reduce bulge when nearing end of progress
    digestLength - progress |> clamp 1 maxBuglgeLength


getStyleClass : Int -> String -> Metabolism -> String
getStyleClass index baseClass metabolism =
    -- Needs corresponding css classes (bulge-n, clockwise, counter-clockwise).
    let
        bulgeClass =
            "bulge-" ++ (index + 1 |> String.fromInt)

        rotationClass =
            if getRotationForIndex index metabolism == Clockwise then
                "clockwise"

            else
                "counter-clockwise"
    in
    if index < maxBuglgeLength then
        String.join " " [ baseClass, bulgeClass, rotationClass ]

    else
        baseClass


isAtDigestingStep : Int -> Metabolism -> Bool
isAtDigestingStep step { isActive, progress, rate } =
    -- For snake head animation
    isActive && progress < step * rate


init : Metabolism
init =
    { progress = 0
    , bulgeRotation = Clockwise
    , isActive = False
    , rate = initialRate
    }
