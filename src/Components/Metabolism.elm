module Components.Metabolism exposing (..)

import Components.Pill exposing (rotation)
import Html exposing (progress)
import Model exposing (BulgeRotation(..), Metabolism)


maxBulgeLength : number
maxBulgeLength =
    -- Bulge size. Needs corresponding css classes (bulge-n).
    4


digestLength : number
digestLength =
    -- Number of tail tiles to visualise the bulge on.
    26


initialRate : number
initialRate =
    -- Number of tail tiles to step through at a time.
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

    else if progress >= digestLength then
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
    let
        softener =
            2

        clamped =
            digestLength - progress |> clamp softener (softener * maxBulgeLength)
    in
    clamped // softener


getStyleClass : Int -> String -> Metabolism -> String
getStyleClass index baseClass metabolism =
    -- Needs corresponding css classes (bulge-n, clockwise, counter-clockwise).
    let
        bulgeLevel =
            index + 1 + maxBulgeLength - getBulgeLength metabolism

        bulgeClass =
            "bulge-" ++ String.fromInt bulgeLevel

        rotationClass =
            if getRotationForIndex index metabolism == Clockwise then
                "clockwise"

            else
                "counter-clockwise"
    in
    if index < maxBulgeLength then
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
