module Generator exposing (..)

import Model exposing (..)
import Random
import Utils exposing (..)


pillPositionGenerator : Snake -> Map -> Random.Generator Position
pillPositionGenerator { head, tail } map =
    let
        freePositions =
            getFreeTilePositions (head :: tail) map

        indexGenerator =
            Random.int 0 (List.length freePositions - 1)

        lookUpPosition i =
            case List.drop i freePositions |> List.head of
                -- Never occurs since lookUp is always successful
                Nothing ->
                    ( 1, 1 )

                Just pos ->
                    pos
    in
    Random.map lookUpPosition indexGenerator


trimmingGenerator : Random.Generator Int
trimmingGenerator =
    Random.int 4 8


positionAndTrimmingGenerator : Snake -> Map -> Bool -> Random.Generator ( Position, Int )
positionAndTrimmingGenerator snake map omitTrimming =
    let
        withoutTrimming pos =
            ( pos, 0 )

        withTrimming pos trim =
            ( pos, trim )
    in
    if omitTrimming then
        Random.map withoutTrimming (pillPositionGenerator snake map)

    else
        Random.map2 withTrimming (pillPositionGenerator snake map) trimmingGenerator
