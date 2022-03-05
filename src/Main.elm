module Main exposing (..)

import Browser
import Browser.Events exposing (onKeyDown)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Model exposing (..)
import Time
import Update exposing (update)
import Utils exposing (..)


viewTile : Snake -> Food -> Bool -> Tile -> Html Msg
viewTile snake food isGameOver tile =
    case tile of
        Wall ->
            span [ class "tile wall" ] []

        Open pos ->
            if snake.head == pos then
                if isGameOver then
                    span [ class "tile snake-dead" ] []

                else
                    span [ class "tile snake-head" ] []

            else if isSnakeHere snake pos then
                if isGameOver then
                    span [ class "tile snake-dead" ] []

                else
                    span [ class "tile snake-body" ] []

            else if isFoodHere food pos then
                span [ class "tile food" ] []

            else
                span [ class "tile open" ] []


viewMap : Model -> Html Msg
viewMap { snake, food, map, state } =
    let
        viewRow row =
            div [ class "row" ] (List.map (viewTile snake food (state == GameOver)) row)
    in
    div [ class "map" ] (List.map viewRow map)


viewScores : Model -> Html Msg
viewScores { score, highscore } =
    div [ class "scores" ]
        [ div [] [ text ("Score: " ++ String.fromInt score) ]
        , div [] [ text ("Highscore: " ++ String.fromInt highscore) ]
        ]


view : Model -> Html Msg
view model =
    let
        scores =
            viewScores model

        map =
            viewMap model
    in
    case model.state of
        Moving _ ->
            div [ class "game" ]
                [ h1 [] [ text "🐍 Snake" ]
                , scores
                , map
                ]

        GameOver ->
            div [ class "game" ]
                [ h1 [] [ text "Game Over!" ]
                , scores
                , map
                , h3 [ onClick StartGame, style "text-decoration" "underline" ] [ text "Restart ?" ]
                ]


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

        _ ->
            Tick


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.state of
        GameOver ->
            Sub.none

        Moving _ ->
            Sub.batch
                [ Time.every 80 (\_ -> Tick)
                , onKeyDown <| Decode.map keyToMsg <| Decode.field "key" Decode.string
                ]


main : Program () Model Msg
main =
    Browser.element
        { init = init 0
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
