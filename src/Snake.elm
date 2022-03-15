module Snake exposing (..)

import Constants exposing (config)
import Model exposing (Direction(..), Map, Pill, Position, Snake)
import Utils exposing (getFreeTilePositions)


grow : Int -> Snake -> Snake
grow stepsTaken snake =
    { snake
        | canGrow = stepsTaken >= config.growthStartAt
        , isGrowing = snake.canGrow
    }


move : Snake -> Snake
move ({ head, tail, isGrowing } as snake) =
    let
        newTail =
            if isGrowing then
                head :: tail

            else
                head :: List.take (List.length tail - 1) tail

        updateWithHead newHead =
            { snake
                | head = newHead
                , tail = newTail
                , isGrowing = False
            }
    in
    case snake.direction of
        Up ->
            Tuple.mapSecond (\v -> v - 1) head |> updateWithHead

        Down ->
            Tuple.mapSecond (\v -> v + 1) head |> updateWithHead

        Left ->
            Tuple.mapFirst (\v -> v - 1) head |> updateWithHead

        Right ->
            Tuple.mapFirst (\v -> v + 1) head |> updateWithHead


turn : Direction -> Snake -> Snake
turn direction snake =
    let
        prevSelectedDirection =
            snake.direction

        isNewDirectionValid =
            ((direction == Up || direction == Down) && (prevSelectedDirection == Left || prevSelectedDirection == Right))
                || ((direction == Left || direction == Right) && (prevSelectedDirection == Up || prevSelectedDirection == Down))

        newDirection =
            if isNewDirectionValid then
                direction

            else
                prevSelectedDirection
    in
    { snake | direction = newDirection }


trim : Snake -> Int -> Snake
trim ({ tail } as snake) amount =
    let
        numKept =
            List.length tail - amount
    in
    { snake
        | tail = List.take numKept tail
        , trimmed = tail |> List.drop numKept |> List.reverse
    }


isTailHere : Snake -> Position -> Bool
isTailHere { tail } pos =
    List.any (\sPos -> sPos == pos) tail


isTrimmedTailHere : Snake -> Position -> Bool
isTrimmedTailHere { trimmed } pos =
    List.any (\sPos -> sPos == pos) trimmed


isInSyncWithDirection : Snake -> Bool
isInSyncWithDirection snake =
    -- When directions are given too quickly, the snake's direction may change twice before position is updated.
    -- This can lead to moving the snake in an invalid direction. E.g: Up -> Left -> Down, while the snake is still facing up.
    let
        ( headX, headY ) =
            snake.head

        prevSelectedDirection =
            snake.direction

        prevMovedDirection =
            case List.head snake.tail of
                Nothing ->
                    prevSelectedDirection

                Just ( tailX, tailY ) ->
                    if headX > tailX then
                        Right

                    else if headY > tailY then
                        Down

                    else if headY < tailY then
                        Up

                    else
                        Left
    in
    prevSelectedDirection == prevMovedDirection


isOnFreeTile : Map -> Snake -> Bool
isOnFreeTile map { head, tail } =
    let
        isHeadOn pos =
            head == pos
    in
    getFreeTilePositions tail map |> List.any isHeadOn


isOnPill : Maybe Pill -> Snake -> Bool
isOnPill pill { head, tail } =
    case pill of
        Nothing ->
            False

        Just { position } ->
            List.any (\pos -> pos == position) (head :: tail)
