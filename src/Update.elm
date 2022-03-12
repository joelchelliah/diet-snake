module Update exposing (update)

import Generator exposing (..)
import List exposing (tail)
import Model exposing (..)
import Random
import Utils exposing (..)


growSnake : Stats -> Snake -> Snake
growSnake { stepsTaken } snake =
    { snake
        | canGrow = stepsTaken >= config.growthStartAt
        , isGrowing = snake.canGrow
    }


moveSnake : Snake -> Snake
moveSnake ({ head, tail, isGrowing } as snake) =
    let
        newTail =
            if isGrowing then
                head :: tail

            else
                head :: List.take (List.length tail - 1) tail

        updateFields newHead =
            { snake
                | head = newHead
                , tail = newTail
                , isGrowing = False
            }
    in
    case snake.direction of
        Up ->
            Tuple.mapSecond (\v -> v - 1) head |> updateFields

        Down ->
            Tuple.mapSecond (\v -> v + 1) head |> updateFields

        Left ->
            Tuple.mapFirst (\v -> v - 1) head |> updateFields

        Right ->
            Tuple.mapFirst (\v -> v + 1) head |> updateFields


turnSnake : Direction -> Snake -> Snake
turnSnake direction snake =
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


trimSnake : Snake -> Int -> Snake
trimSnake ({ tail } as snake) trim =
    let
        numKept =
            List.length tail - trim
    in
    { snake
        | tail = List.take numKept tail
        , trimmed = tail |> List.drop numKept |> List.reverse
    }



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


getBestStats : Model -> Stats
getBestStats { stats, bestStats } =
    let
        newWeightLoss =
            if stats.weightLoss > bestStats.weightLoss then
                stats.weightLoss

            else
                bestStats.weightLoss

        newStepsTaken =
            if stats.stepsTaken > bestStats.stepsTaken then
                stats.stepsTaken

            else
                bestStats.stepsTaken

        newPillsTaken =
            if stats.pillsTaken > bestStats.pillsTaken then
                stats.pillsTaken

            else
                bestStats.pillsTaken
    in
    { bestStats | weightLoss = newWeightLoss, stepsTaken = newStepsTaken, pillsTaken = newPillsTaken }


updateWhileActive : Msg -> Model -> ( Model, Cmd Msg )
updateWhileActive msg ({ snake, state, pill, map, stats, bestStats } as model) =
    case msg of
        StartGame ->
            init bestStats ()

        Enter ->
            if state == Paused || state == Init then
                ( { model | state = Running }, Cmd.none )

            else if state == GameOver then
                updateWhileActive StartGame model

            else
                ( { model | state = Paused }, Cmd.none )

        KeyPress direction ->
            let
                newSnake =
                    if isSnakePositionInSyncWithSnakeDirection snake then
                        turnSnake direction snake

                    else
                        snake |> moveSnake |> turnSnake direction
            in
            ( { model | snake = newSnake }, Cmd.none )

        Grow ->
            ( { model | snake = growSnake stats snake }, Cmd.none )

        Tick ->
            let
                newSnake =
                    moveSnake snake

                toNewPillMsg ( pos, trim ) =
                    NewPillAndSnakeTrimming pos trim

                newStats =
                    if isSnakeOnPill newSnake pill then
                        { stats | stepsTaken = stats.stepsTaken + 1, pillsTaken = stats.pillsTaken + 1 }

                    else
                        { stats | stepsTaken = stats.stepsTaken + 1 }

                newCommand =
                    if isSnakeOnPill newSnake pill then
                        Random.generate toNewPillMsg (positionAndTrimmingGenerator newSnake map False)

                    else if pill == Nothing then
                        Random.generate toNewPillMsg (positionAndTrimmingGenerator newSnake map True)

                    else
                        Cmd.none
            in
            if isSnakeOnFreeTile newSnake map then
                ( { model | snake = newSnake, stats = newStats }, newCommand )

            else
                ( { model | state = GameOver, bestStats = getBestStats model }, Cmd.none )

        NewPillAndSnakeTrimming pos trim ->
            let
                newSnake =
                    trimSnake snake trim

                newStats =
                    { stats | weightLoss = stats.weightLoss + trim }
            in
            ( { model | pill = Just pos, snake = newSnake, stats = newStats }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ state } as model) =
    let
        isInactive =
            List.member state [ Init, Paused, GameOver ]
    in
    if isInactive && msg /= Enter then
        ( model, Cmd.none )

    else
        updateWhileActive msg model
