module Main exposing (..)

import Browser
import Browser.Events exposing (onKeyDown)
import Components.Hud
import Components.Snake
import Components.Stats
import Components.Tiles
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Json.Decode as Decode
import Message exposing (getNewPillAndTrimCommand, keyToMessage)
import Model exposing (Direction(..), GameState(..), Model, Msg(..), StatDetails, Tile(..), config)
import Time
import Utils.Icon exposing (CornerIcon(..), iconCss)


init : StatDetails -> () -> ( Model, Cmd Msg )
init bestStats () =
    ( { snake = Components.Snake.init 5
      , pill = Nothing
      , state = Init
      , map = Components.Tiles.init config.gameWidth config.gameHeight
      , stats = Components.Stats.init bestStats
      }
    , Cmd.none
    )


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


view : Model -> Html Msg
view model =
    div [ class "game" ]
        [ iconCss
        , Components.Hud.viewHeader model
        , Components.Tiles.view model
        , Components.Stats.view model.stats
        , Components.Hud.viewModal model
        , Components.Hud.viewFooter
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        keyDownSubscription =
            Decode.field "key" Decode.string |> Decode.map keyToMessage |> onKeyDown
    in
    case model.state of
        Running ->
            Sub.batch
                [ Time.every config.gameSpeed (\_ -> Tick)
                , Time.every config.growthRate (\_ -> Grow)
                , keyDownSubscription
                ]

        _ ->
            keyDownSubscription


main : Program () Model Msg
main =
    Browser.element
        { init = init Components.Stats.initDetails
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
