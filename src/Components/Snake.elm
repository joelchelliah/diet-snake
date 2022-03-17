module Components.Snake exposing (..)

import Model exposing (Direction(..), Map, Pill, Position, Snake, config)
import Utils.Position exposing (getFreeTilePositions)


disabledDigestingProgress : number
disabledDigestingProgress =
    -- Disables digesting state without requiring an extra field
    -1


getDigestBulgeLength : Snake -> Int
getDigestBulgeLength snake =
    -- To slowly reduce bulge when nearing end of progress
    config.digestLength - snake.digestingProgress |> clamp 1 5


validateDirection : Direction -> Direction -> Direction
validateDirection currentDirection newDirection =
    let
        isNewDirectionValid =
            ((newDirection == Up || newDirection == Down) && (currentDirection == Left || currentDirection == Right))
                || ((newDirection == Left || newDirection == Right) && (currentDirection == Up || currentDirection == Down))
    in
    if isNewDirectionValid then
        newDirection

    else
        currentDirection


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
turn newDirection snake =
    let
        syncedSnake =
            if isInSyncWithDirection snake then
                snake

            else
                move snake

        validNewDirection =
            validateDirection snake.direction newDirection
    in
    { syncedSnake | direction = validNewDirection }


digest : Bool -> Snake -> Snake
digest onPill ({ digestingProgress } as snake) =
    if onPill then
        { snake | digestingProgress = 0 }

    else if isDigesting snake then
        { snake | digestingProgress = digestingProgress + config.digestRate }

    else
        { snake | digestingProgress = disabledDigestingProgress }


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


isDigesting : Snake -> Bool
isDigesting { digestingProgress } =
    digestingProgress >= 0 && digestingProgress <= config.digestLength


isAtDigestingStep : Int -> Snake -> Bool
isAtDigestingStep step snake =
    isDigesting snake && snake.digestingProgress < step * config.digestRate


getCurrentlyDigestingTailPortion : Snake -> List Position
getCurrentlyDigestingTailPortion ({ tail, digestingProgress } as snake) =
    let
        bulge =
            getDigestBulgeLength snake
    in
    if isDigesting snake then
        List.drop digestingProgress tail |> List.take bulge

    else
        []


init : Int -> Snake
init tailLength =
    let
        head =
            ( config.gameWidth // 2, config.gameHeight // 2 )

        createTail fromLength =
            if fromLength == tailLength then
                []

            else
                Tuple.mapSecond (\y -> y + fromLength) head :: createTail (fromLength + 1)
    in
    { head = head
    , tail = createTail 0
    , trimmed = []
    , direction = Up
    , isGrowing = False
    , canGrow = False
    , digestingProgress = disabledDigestingProgress
    }
