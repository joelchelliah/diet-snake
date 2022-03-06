module Main exposing (..)

import Browser
import Browser.Events exposing (onKeyDown)
import FontAwesome.Attributes as Icon
import FontAwesome.Brands as Icon
import FontAwesome.Icon as Icon exposing (Icon)
import FontAwesome.Layering as Icon
import FontAwesome.Solid as Icon
import FontAwesome.Styles as Icon
import FontAwesome.Svg as SvgIcon
import FontAwesome.Transforms as Icon
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Model exposing (..)
import Time
import Update exposing (update)
import Utils exposing (..)



-- TODO: Icon module ???
-- TODO: Discard gradually disappearing (css?)
-- TODO: Update score
-- TODO: modal shadows


viewTile : Snake -> Pill -> List Position -> Bool -> Tile -> Html Msg
viewTile snake pill discardedSnake isGameOver tile =
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

            else if isPillHere pill pos then
                span [ class "tile pill" ] []

            else if List.any (\discard -> pos == discard) discardedSnake then
                span [ class "tile snake-dead" ] []

            else
                span [ class "tile open" ] []


viewMap : Model -> Html Msg
viewMap { snake, pill, discardedSnake, map, state } =
    let
        viewRow row =
            div [ class "row" ] (List.map (viewTile snake pill discardedSnake (state == GameOver)) row)
    in
    div [ class "map" ] (List.map viewRow map)


viewTitle : Html Msg
viewTitle =
    div [ class "header" ]
        [ div [ class "header-icon" ] [ text "🐍" ]
        , div [ class "header-texts" ]
            [ div [ class "header-texts-title" ] [ text "Diet Snake" ]
            , div [ class "header-texts-subtitle" ] [ text "The totally backward snake game!" ]
            ]
        ]


viewScores : Model -> Html Msg
viewScores { stats, bestStats } =
    div [ class "scores" ]
        [ div [] [ text ("Current weight loss: " ++ String.fromInt stats.weightLoss) ]
        , div [] [ text ("Most weight lost: " ++ String.fromInt bestStats.weightLoss) ]
        ]


viewModal : Model -> Html Msg
viewModal { state } =
    case state of
        Init ->
            div [ class "modal" ]
                [ div [ class "modal-title" ] [ text "New Game" ]

                -- , text "Mr. Snake is growing too fast."
                -- , p [] [ text "Help him lose weight by guiding him towards his diet supplements, and ensure that he lives a long and prosperous life." ]
                , text "Press "
                , b [] [ i [] [ text "Enter" ] ]
                , text " to start the diet!"
                ]

        Paused ->
            div [ class "modal" ]
                [ div [ class "modal-title" ] [ text "Paused" ]
                , text "Press "
                , b [] [ i [] [ text "Enter" ] ]
                , text " to resume the diet"
                ]

        GameOver ->
            div [ class "modal" ]
                [ div [ class "game-over-title" ] [ text "Snake is dead!" ]
                , a [ href "", onClick StartGame, style "text-decoration" "underline" ] [ text "Try again?" ]
                ]

        _ ->
            span [] []


viewInstructions : Html Msg
viewInstructions =
    let
        viewIcon i =
            i |> Icon.present |> Icon.transform [ Icon.shrink 2 ] |> Icon.styled [ Icon.lg ] |> Icon.view

        icons =
            List.map viewIcon
                [ Icon.arrowCircleUp, Icon.arrowCircleRight, Icon.arrowCircleDown, Icon.arrowCircleLeft ]
    in
    div [ class "instructions" ]
        [ p []
            [ b [] [ text "Oh no! " ]
            , text "Mr. Snake is growing too fast."
            ]
        , p []
            [ text "Help him lose weight by guiding him towards his diet supplements, and ensure that he lives a long and prosperous life."
            , div [ class "instructions-arrow-icons" ] icons
            , text "Move around using the Arrow keys."
            ]
        ]


viewInfo : Html Msg
viewInfo =
    let
        url =
            "https://github.com/joelchelliah/diet-snake"

        icon =
            Icon.github |> Icon.present |> Icon.styled [ Icon.lg, Icon.pullLeft ] |> Icon.view
    in
    div
        [ class "info" ]
        [ icon, a [ href url ] [ text "Find it on Github" ] ]


view : Model -> Html Msg
view model =
    div [ class "game" ]
        [ Icon.css
        , viewTitle
        , viewScores model
        , viewMap model
        , viewModal model
        , viewInstructions
        , viewInfo
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

        "Enter" ->
            Pause

        _ ->
            Tick


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.state of
        GameOver ->
            Sub.none

        _ ->
            Sub.batch
                [ Time.every 100 (\_ -> Tick)
                , Time.every 300 (\_ -> Grow)
                , onKeyDown <| Decode.map keyToMsg <| Decode.field "key" Decode.string
                ]


main : Program () Model Msg
main =
    Browser.element
        { init = init initStats
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
