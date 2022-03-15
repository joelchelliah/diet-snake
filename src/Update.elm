module Update exposing (update)

import Command exposing (getNewPillAndTrimCommand)
import Constants exposing (..)
import Model exposing (..)
import Snake
import Stats
import Utils exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ snake, state, pill, map, stats } as model) =
    if List.member state [ Init, Paused, GameOver ] && msg /= Enter then
        ( model, Cmd.none )

    else
        case msg of
            StartGame ->
                restart stats

            Enter ->
                if state == Paused || state == Init then
                    unpause model

                else if state == GameOver then
                    restart stats

                else
                    pause model

            KeyPress direction ->
                ( { model | snake = Snake.turn direction snake }, Cmd.none )

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
                        , stats = Stats.updateCurrent onPill 0 stats
                      }
                    , getNewPillAndTrimCommand newSnake pill map
                    )

                else
                    ( { model
                        | state = GameOver
                        , stats = Stats.updateBest stats
                      }
                    , Cmd.none
                    )

            NewPill newPill ->
                ( { model | pill = Just newPill }, Cmd.none )

            Trim amount ->
                ( { model
                    | snake = Snake.trim snake amount
                    , stats = Stats.updateCurrent False amount stats
                  }
                , Cmd.none
                )


restart : Stats -> ( Model, Cmd Msg )
restart stats =
    init stats.best ()


pause : Model -> ( Model, Cmd msg )
pause model =
    ( { model | state = Paused }, Cmd.none )


unpause : Model -> ( Model, Cmd msg )
unpause model =
    ( { model | state = Running }, Cmd.none )
