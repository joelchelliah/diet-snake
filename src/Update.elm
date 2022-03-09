module Update exposing (update)

import Generator exposing (..)
import List exposing (tail)
import Model exposing (..)
import Random
import Utils exposing (..)


moveSnakeAndUpdateDiscard : Snake -> Snake
moveSnakeAndUpdateDiscard snake =
    let
        { head, tail, isGrowing, discard } =
            snake

        newTail =
            if isGrowing then
                head :: tail

            else
                head :: List.take (List.length tail - 1) tail

        -- Discard delay to ensure that css animation is finished running before removing
        discardDelay =
            10

        newDiscard =
            if List.length discard > discardDelay then
                List.drop 1 discard

            else
                discard

        updateFields newHead =
            { snake
                | head = newHead
                , tail = newTail
                , canGrow = True
                , isGrowing = False
                , discard = newDiscard
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


turnSnake : Snake -> Direction -> Snake
turnSnake snake direction =
    let
        currentDirection =
            snake.direction

        isValid =
            ((direction == Up || direction == Down) && (currentDirection == Left || currentDirection == Right))
                || ((direction == Left || direction == Right) && (currentDirection == Up || currentDirection == Down))

        newDirection =
            if isValid then
                direction

            else
                currentDirection
    in
    { snake | direction = newDirection }


trimSnake : Snake -> Int -> Snake
trimSnake ({ tail, discard } as snake) trim =
    let
        numKept =
            List.length tail - trim
    in
    { snake
        | tail = List.take numKept tail
        , discard = List.drop numKept tail |> List.reverse |> List.append discard
    }


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ snake, state, pill, map, stats, bestStats } as model) =
    let
        isPaused =
            state == Paused || state == Init

        isDead =
            state == GameOver
    in
    case msg of
        StartGame ->
            init bestStats ()

        Enter ->
            if isPaused then
                ( { model | state = Running }, Cmd.none )

            else if isDead then
                update StartGame model

            else
                ( { model | state = Paused }, Cmd.none )

        KeyPress direction ->
            ( { model | snake = turnSnake snake direction }, Cmd.none )

        Grow ->
            ( { model | snake = { snake | isGrowing = snake.canGrow } }, Cmd.none )

        Tick ->
            let
                newSnake =
                    moveSnakeAndUpdateDiscard snake

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
            if isPaused then
                ( model, Cmd.none )

            else if isSnakeOnFreeTile newSnake map then
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
