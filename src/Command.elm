module Command exposing (getNewPillAndTrimCommand)

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
            case lookUpInList i freePositions of
                -- Never occurs since lookUp is always successful
                Nothing ->
                    ( 1, 1 )

                Just pos ->
                    pos
    in
    Random.map lookUpPosition indexGenerator


trimmingGenerator : Snake -> Random.Generator Int
trimmingGenerator snake =
    let
        tailLength =
            List.length snake.tail

        min =
            tailLength // 8

        max =
            tailLength // 4
    in
    Random.int (clamp 2 max min) (clamp min 10 max)


getNewPillAndTrimCommand : Snake -> Pill -> Map -> Cmd Msg
getNewPillAndTrimCommand snake pill map =
    let
        newPillCmd =
            Random.generate NewPill (pillPositionGenerator snake map)

        trimCmd =
            Random.generate Trim (trimmingGenerator snake)
    in
    if isSnakeOnPill snake pill then
        Cmd.batch [ newPillCmd, trimCmd ]

    else if pill == Nothing then
        newPillCmd

    else
        Cmd.none
