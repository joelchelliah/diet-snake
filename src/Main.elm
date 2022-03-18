module Main exposing (..)

import Browser
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown)
import Components.Hud as Hud
import Components.Snake as Snake
import Components.Stats as Stats
import Components.Tiles as Tiles
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Json.Decode as Decode
import Message exposing (getNewPillAndTrimCommand, keyToMessage)
import Model exposing (Direction(..), GameState(..), Model, Msg(..), StatDetails, Tile(..), config)
import Utils.Icon exposing (CornerIcon(..), iconCss)


init : StatDetails -> () -> ( Model, Cmd Msg )
init bestStats () =
    ( { snake = Snake.init 5
      , pill = Nothing
      , state = Init
      , map = Tiles.init config.gameWidth config.gameHeight
      , stats = Stats.init bestStats
      , frameDeltas =
            { tickDelta = 0
            , growDelta = 0
            }
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ snake, state, pill, map, stats, frameDeltas } as model) =
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
                        | snake = Snake.digest onPill newSnake
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

            FrameDelta delta ->
                let
                    newGrowDelta =
                        frameDeltas.growDelta + delta

                    newTickDelta =
                        frameDeltas.tickDelta + delta

                    updateDeltas grow tick =
                        { frameDeltas | growDelta = grow, tickDelta = tick }
                in
                if newGrowDelta >= config.growthRate then
                    update Grow
                        { model | frameDeltas = updateDeltas 0 newTickDelta }

                else if newTickDelta >= config.gameSpeed then
                    update Tick
                        { model | frameDeltas = updateDeltas newGrowDelta 0 }

                else
                    ( { model | frameDeltas = updateDeltas newGrowDelta newTickDelta }, Cmd.none )


view : Model -> Html Msg
view model =
    div [ class "game" ]
        [ iconCss
        , Hud.viewHeader model
        , Tiles.view model
        , Stats.view model.stats
        , Hud.viewModal model
        , Hud.viewFooter
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
                [ onAnimationFrameDelta FrameDelta
                , keyDownSubscription
                ]

        _ ->
            keyDownSubscription


main : Program () Model Msg
main =
    Browser.element
        { init = init Stats.initDetails
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
