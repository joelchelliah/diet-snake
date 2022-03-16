module Subscription exposing (subscriptions)

import Browser.Events exposing (..)
import Json.Decode as Decode
import Model exposing (Direction(..), GameState(..), Model, Msg(..), config)
import Time


keyToMsg : String -> Msg
keyToMsg string =
    case string of
        "ArrowUp" ->
            KeyPress Up

        "ArrowRight" ->
            KeyPress Right

        "ArrowDown" ->
            KeyPress Down

        "ArrowLeft" ->
            KeyPress Left

        "Enter" ->
            Enter

        _ ->
            Tick


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        keyDownSubscription =
            onKeyDown <| Decode.map keyToMsg <| Decode.field "key" Decode.string
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
