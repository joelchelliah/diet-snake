module Update exposing (update)

import Command exposing (getNewPillAndTrimCommand)
import Constants exposing (..)
import Model exposing (..)
import Snake
import Utils exposing (..)


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
                        if Snake.isInSyncWithDirection snake then
                            Snake.turn direction snake

                        else
                            snake |> Snake.move |> Snake.turn direction
                in
                ( { model | snake = newSnake }, Cmd.none )

            Grow ->
                ( { model | snake = Snake.grow stats.current.stepsTaken snake }, Cmd.none )

            Tick ->
                let
                    newSnake =
                        Snake.move snake

                    onPill =
                        Snake.isOnPill pill newSnake
                in
                if Snake.isOnFreeTile map newSnake then
                    ( { model
                        | snake = newSnake
                        , stats = updateCurrentStats onPill 0 stats
                      }
                    , getNewPillAndTrimCommand newSnake pill map
                    )

                else
                    ( { model | state = GameOver, stats = updateBestStats stats }, Cmd.none )

            NewPill newPill ->
                ( { model | pill = Just newPill }, Cmd.none )

            Trim amount ->
                ( { model
                    | snake = Snake.trim snake amount
                    , stats = updateCurrentStats False amount stats
                  }
                , Cmd.none
                )
