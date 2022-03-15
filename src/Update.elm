module Update exposing (update)

import Command exposing (getNewPillAndTrimCommand)
import Constants exposing (..)
import Model exposing (..)
import Utils exposing (..)


growSnake : Int -> Snake -> Snake
growSnake stepsTaken snake =
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


updateBestStats : Stats -> Stats
updateBestStats ({ current, best } as stats) =
    let
        newWeightLoss =
            if current.weightLoss > best.weightLoss then
                current.weightLoss

            else
                best.weightLoss

        newStepsTaken =
            if current.stepsTaken > best.stepsTaken then
                current.stepsTaken

            else
                best.stepsTaken

        newPillsTaken =
            if current.pillsTaken > best.pillsTaken then
                current.pillsTaken

            else
                best.pillsTaken
    in
    { stats
        | best =
            { best
                | weightLoss = newWeightLoss
                , stepsTaken = newStepsTaken
                , pillsTaken = newPillsTaken
            }
    }


updateCurrentStats : Bool -> Int -> Stats -> Stats
updateCurrentStats tookPill weightLoss ({ current } as stats) =
    let
        pillsTaken =
            if tookPill then
                1

            else
                0

        newCurrent =
            { current
                | stepsTaken = current.stepsTaken + 1
                , pillsTaken = current.pillsTaken + pillsTaken
                , weightLoss = current.weightLoss + weightLoss
            }
    in
    { stats | current = newCurrent }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ snake, state, pill, map, stats } as model) =
    if List.member state [ Init, Paused, GameOver ] && msg /= Enter then
        ( model, Cmd.none )

    else
        case msg of
            StartGame ->
                init stats.best ()

            Enter ->
                if state == Paused || state == Init then
                    ( { model | state = Running }, Cmd.none )

                else if state == GameOver then
                    init stats.best ()

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
                ( { model | snake = growSnake stats.current.stepsTaken snake }, Cmd.none )

            Tick ->
                let
                    newSnake =
                        moveSnake snake

                    tookPill =
                        isSnakeOnPill newSnake pill
                in
                if isSnakeOnFreeTile newSnake map then
                    ( { model
                        | snake = newSnake
                        , stats = updateCurrentStats tookPill 0 stats
                      }
                    , getNewPillAndTrimCommand newSnake pill map
                    )

                else
                    ( { model | state = GameOver, stats = updateBestStats stats }, Cmd.none )

            NewPill newPill ->
                ( { model | pill = Just newPill }, Cmd.none )

            Trim amount ->
                ( { model
                    | snake = trimSnake snake amount
                    , stats = updateCurrentStats False amount stats
                  }
                , Cmd.none
                )
