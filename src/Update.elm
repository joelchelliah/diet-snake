module Update exposing (update)

import Command exposing (getNewPillAndTrimCommand)
import Components.Snake
import Components.Stats
import Init exposing (init)
import Model exposing (GameState(..), Model, Msg(..), Stats)


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
                ( { model | snake = Components.Snake.turn direction snake }, Cmd.none )

            Grow ->
                ( { model | snake = Components.Snake.grow stats.current.stepsTaken snake }, Cmd.none )

            Tick ->
                let
                    newSnake =
                        Components.Snake.move snake

                    onPill =
                        Components.Snake.isOnPill pill newSnake
                in
                if Components.Snake.isOnFreeTile map newSnake then
                    ( { model
                        | snake = newSnake
                        , stats = Components.Stats.updateCurrent onPill 0 stats
                      }
                    , getNewPillAndTrimCommand newSnake pill map
                    )

                else
                    ( { model
                        | state = GameOver
                        , stats = Components.Stats.updateBest stats
                      }
                    , Cmd.none
                    )

            NewPill newPill ->
                ( { model | pill = Just newPill }, Cmd.none )

            Trim amount ->
                ( { model
                    | snake = Components.Snake.trim snake amount
                    , stats = Components.Stats.updateCurrent False amount stats
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
