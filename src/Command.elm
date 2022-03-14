module Command exposing (getNewPillAndTrimCommand)

import Model exposing (..)
import Random
import Utils exposing (..)


pillColor =
    { green = "green"
    , blue = "blue"
    , yellow = "yellow"
    , pink = "pink"
    , teal = "teal"
    }


allPillColors =
    [ pillColor.green, pillColor.blue, pillColor.yellow, pillColor.pink, pillColor.teal ]


getPositionGenerator : Snake -> Map -> Random.Generator Position
getPositionGenerator { head, tail } map =
    let
        freePositions =
            getFreeTilePositions (head :: tail) map

        indexGenerator =
            Random.int 0 (List.length freePositions - 1)

        lookUpPosition =
            lookUpInListOrDefault freePositions ( 4, 2 )
    in
    Random.map lookUpPosition indexGenerator


getColorGenerator : Maybe Pill -> Random.Generator String
getColorGenerator maybePill =
    let
        colors =
            case maybePill of
                Nothing ->
                    allPillColors

                Just pill ->
                    List.filter (\col -> col /= pill.color) allPillColors

        colorIndexGenerator =
            Random.int 0 (List.length colors - 1)
    in
    Random.map (lookUpInListOrDefault colors pillColor.green) colorIndexGenerator


getPillGenerator : Snake -> Maybe Pill -> Map -> Random.Generator Pill
getPillGenerator snake pill map =
    Random.map2
        (\pos col -> { position = pos, color = col })
        (getPositionGenerator snake map)
        (getColorGenerator pill)


getTrimGenerator : Snake -> Random.Generator Int
getTrimGenerator snake =
    let
        tailLength =
            List.length snake.tail

        min =
            tailLength // 8

        max =
            tailLength // 4
    in
    Random.int (clamp 2 max min) (clamp min 12 max)


getNewPillAndTrimCommand : Snake -> Maybe Pill -> Map -> Cmd Msg
getNewPillAndTrimCommand snake pill map =
    let
        newPillCmd =
            Random.generate NewPill (getPillGenerator snake pill map)

        trimCmd =
            Random.generate Trim (getTrimGenerator snake)
    in
    if isSnakeOnPill snake pill then
        Cmd.batch [ newPillCmd, trimCmd ]

    else if pill == Nothing then
        newPillCmd

    else
        Cmd.none
