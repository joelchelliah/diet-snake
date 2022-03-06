module Update exposing (update)

import Generator exposing (..)
import List exposing (tail)
import Model exposing (..)
import Random
import Utils exposing (..)


moveSnake : Snake -> GameState -> Snake
moveSnake snake state =
    let
        { head, tail, isGrowing } =
            snake

        newTail =
            if isGrowing then
                head :: tail

            else
                head :: List.take (List.length tail - 1) tail

        updateFields newHead =
            { snake
                | head = newHead
                , tail = newTail
                , canGrow = True
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


trimSnake : Snake -> Int -> ( Snake, List Position )
trimSnake ({ tail } as snake) trim =
    let
        numKept =
            List.length tail - trim

        newTail =
            List.take numKept tail

        discard =
            List.drop numKept tail
    in
    ( { snake | tail = newTail }, discard )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ snake, state, pill, map, stats, bestStats } as model) =
    let
        isPaused =
            state == Paused || state == Init
    in
    case msg of
        StartGame ->
            let
                newWeightLoss =
                    if stats.weightLoss > bestStats.weightLoss then
                        stats.weightLoss

                    else
                        bestStats.weightLoss
            in
            init { bestStats | weightLoss = newWeightLoss } ()

        Pause ->
            if isPaused then
                ( { model | state = Running }, Cmd.none )

            else
                ( { model | state = Paused }, Cmd.none )

        KeyPress direction ->
            ( { model | snake = turnSnake snake direction }, Cmd.none )

        Grow ->
            ( { model | snake = { snake | isGrowing = snake.canGrow } }, Cmd.none )

        Tick ->
            let
                newSnake =
                    moveSnake snake state

                toNewPillMsg ( pos, trim ) =
                    NewPillAndTrimSnake pos trim

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
                ( { model | snake = newSnake }, newCommand )

            else
                ( { model | state = GameOver }, Cmd.none )

        NewPillAndTrimSnake pos trim ->
            let
                ( newSnake, discard ) =
                    trimSnake snake trim
            in
            ( { model | pill = Just pos, snake = newSnake, discardedSnake = discard }, Cmd.none )
