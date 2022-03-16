module Command exposing (getNewPillAndTrimCommand)

import Components.Pill
import Components.Snake
import Random
import Types exposing (Map, Msg(..), Pill, PillColor(..), Position, Snake)
import Utils.ListExtra exposing (lookUpInListOrDefault)
import Utils.Position exposing (getFreeTilePositions)


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


getColorGenerator : Maybe Pill -> Random.Generator PillColor
getColorGenerator maybePill =
    let
        colors =
            case maybePill of
                Nothing ->
                    Components.Pill.colors

                Just pill ->
                    Components.Pill.getAllColorsExceptPillColor pill

        indexGenerator =
            Random.int 0 (List.length colors - 1)
    in
    Random.map (lookUpInListOrDefault colors Green) indexGenerator


getRotationGenerator : Maybe Pill -> Random.Generator Float
getRotationGenerator maybePill =
    let
        { min, max, multiplier } =
            Components.Pill.rotation

        isCurrentPillRotation rotation =
            case maybePill of
                Nothing ->
                    False

                Just pill ->
                    pill.rotation == rotation
    in
    Random.float min max
        |> Random.map (\val -> val * multiplier)
        |> Random.map
            (\rot ->
                if isCurrentPillRotation rot then
                    rot - Components.Pill.rotation.multiplier

                else
                    rot
            )


getPillGenerator : Snake -> Maybe Pill -> Map -> Random.Generator Pill
getPillGenerator snake pill map =
    Random.map3
        (\pos col rot -> { position = pos, color = col, rotation = rot })
        (getPositionGenerator snake map)
        (getColorGenerator pill)
        (getRotationGenerator pill)


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
    if Components.Snake.isOnPill pill snake then
        Cmd.batch [ newPillCmd, trimCmd ]

    else if pill == Nothing then
        newPillCmd

    else
        Cmd.none
