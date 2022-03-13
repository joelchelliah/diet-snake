module Update exposing (update)

import Command exposing (getNewPillAndTrimCommand)
import Model exposing (..)
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
        isInactive =
            List.member state [ Init, Paused, GameOver ]
    in
    if isInactive && msg /= Enter then
        ( model, Cmd.none )

    else
        case msg of
            StartGame ->
                init bestStats ()

            Enter ->
                if state == Paused || state == Init then
                    ( { model | state = Running }, Cmd.none )

                else if state == GameOver then
                    update StartGame model

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

                    newStats =
                        if isSnakeOnPill newSnake pill then
                            { stats | stepsTaken = stats.stepsTaken + 1, pillsTaken = stats.pillsTaken + 1 }

                        else
                            { stats | stepsTaken = stats.stepsTaken + 1 }
                in
                if isSnakeOnFreeTile newSnake map then
                    ( { model | snake = newSnake, stats = newStats }
                    , getNewPillAndTrimCommand newSnake pill map
                    )

                else
                    ( { model | state = GameOver, bestStats = getBestStats model }, Cmd.none )

            NewPill pos ->
                ( { model | pill = Just pos }, Cmd.none )

            Trim amount ->
                ( { model
                    | snake = trimSnake snake amount
                    , stats = { stats | weightLoss = stats.weightLoss + amount }
                  }
                , Cmd.none
                )
