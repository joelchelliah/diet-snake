module Utils exposing (..)

import Model exposing (..)


isPillHere : Pill -> Position -> Bool
isPillHere pill pos =
    case pill of
        Nothing ->
            False

        Just pillPos ->
            pillPos == pos


isSnakeHere : Snake -> Position -> Bool
isSnakeHere { head, tail } pos =
    List.any (\sPos -> sPos == pos) (head :: tail)


isTrimmedAwaySnakeHere : Snake -> Position -> Bool
isTrimmedAwaySnakeHere { trimmed } pos =
    List.any (\sPos -> sPos == pos) trimmed


isSnakeOnPill : Snake -> Pill -> Bool
isSnakeOnPill { head } pill =
    isPillHere pill head


isSnakeOnFreeTile : Snake -> Map -> Bool
isSnakeOnFreeTile { head, tail } map =
    let
        isHeadOn pos =
            head == pos
    in
    getFreeTilePositions tail map |> List.any isHeadOn



-- When directions are given too quickly, the snake's direction may change twice before position is updated.
-- This can lead to moving the snake in an invalid direction. E.g: Up -> Left -> Down, while the snake is still facing up.
-- Checking for this case here:


isSnakePositionInSyncWithSnakeDirection : Snake -> Bool
isSnakePositionInSyncWithSnakeDirection snake =
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


getNonWallPositions : Map -> List Position
getNonWallPositions =
    let
        getFromRow =
            List.filterMap
                (\tile ->
                    case tile of
                        Wall ->
                            Nothing

                        Open pos ->
                            Just pos
                )
    in
    List.foldl (\row acc -> getFromRow row |> List.append acc) []


getFreeTilePositions : List Position -> Map -> List Position
getFreeTilePositions nonFreePositions map =
    let
        nonWallPositions =
            getNonWallPositions map
    in
    List.filter (\pos -> not <| List.member pos nonFreePositions) nonWallPositions


lookUpInList : Int -> List a -> Maybe a
lookUpInList i list =
    List.drop i list |> List.head


getIndexInList : a -> List a -> Int
getIndexInList a list =
    case list of
        head :: tail ->
            if head == a then
                0

            else
                1 + getIndexInList a tail

        _ ->
            0
