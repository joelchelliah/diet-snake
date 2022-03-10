module Main exposing (..)

import Animation exposing (..)
import Browser
import Browser.Events exposing (onKeyDown)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Icon exposing (..)
import Json.Decode as Decode
import Model exposing (..)
import Simple.Animation.Animated exposing (ClassName)
import Time
import Update exposing (update)
import Utils exposing (..)


viewTile : Snake -> Pill -> Bool -> Tile -> Html Msg
viewTile snake pill isGameOver tile =
    let
        makeTile className inerTileContainer =
            case className of
                "" ->
                    div [ class "tile outer-tile" ] [ inerTileContainer [] [] ]

                name ->
                    div [ class "tile outer-tile" ] [ inerTileContainer [ class ("tile inner-tile " ++ name) ] [] ]
    in
    case tile of
        Wall ->
            makeTile "wall" div

        Open pos ->
            if snake.head == pos then
                if isGameOver then
                    makeTile "snake-dead" div

                else
                    makeTile "snake-head" div

            else if isSnakeHere snake pos then
                if isGameOver then
                    makeTile "snake-dead" div

                else
                    makeTile "snake-body" div

            else if isPillHere pill pos then
                makeTile "pill" div

            else if List.any (\dis -> pos == dis) snake.discard then
                makeTile "snake-dead" fadeAndShrinkAway

            else
                makeTile "" span


viewMap : Model -> Html Msg
viewMap { snake, pill, map, state } =
    let
        viewRow row =
            div [ class "row" ] (List.map (viewTile snake pill (state == GameOver)) row)
    in
    div [ class "map" ] (List.map viewRow map)


viewTitle : GameState -> Html Msg
viewTitle state =
    let
        subTitleText =
            if state == GameOver then
                text "Ouch! ...Try again?"

            else
                text "The totally backward snake game!"

        titleText =
            if state == GameOver then
                span [ class "blue-text" ]
                    [ span [ class "red-text" ] [ text "D" ]
                    , s [ class "grey-text" ] [ text "iet" ]
                    , span [ class "red-text" ] [ text "ead " ]
                    , text "Snake"
                    ]

            else
                span [ class "blue-text" ] [ text "Diet Snake" ]
    in
    div [ class "header" ]
        [ div [ class "header-icon" ] [ text "🐍" ]
        , div [ class "header-texts" ]
            [ div [ class "header-texts-title" ] [ titleText ]
            , div [ class "header-texts-subtitle" ] [ subTitleText ]
            ]
        ]


viewScoreBoard : Model -> Html Msg
viewScoreBoard { stats, bestStats } =
    div [ class "scoreboard-row" ]
        [ div [ class "scoreboard", style "align-items" "flex-start" ]
            [ div [] [ text ("Steps taken: " ++ String.fromInt stats.stepsTaken) ]
            , div [] [ text ("Pills consumed: " ++ String.fromInt stats.pillsTaken) ]
            , div [] [ text ("Weight lost: " ++ String.fromInt stats.weightLoss) ]
            ]
        , div [ class "scoreboard", style "align-items" "flex-end" ]
            [ div [] [ text ("Most steps taken: " ++ String.fromInt bestStats.stepsTaken) ]
            , div [] [ text ("Most pills consumed: " ++ String.fromInt bestStats.pillsTaken) ]
            , div [] [ text ("Most weight lost: " ++ String.fromInt bestStats.weightLoss) ]
            ]
        ]


viewPressEnterTo : String -> Html Msg
viewPressEnterTo reason =
    div []
        [ text "Press "
        , b [] [ i [ class "blue-text" ] [ text "Enter" ] ]
        , text (" to " ++ reason)
        ]


viewModal : Model -> Html Msg
viewModal { state } =
    case state of
        Init ->
            div [ class "modal" ]
                [ div [ class "modal-title" ] [ text "- Oh no -" ]
                , div [ class "modal-text" ]
                    [ p [] [ text "Mr. Snake is growing too fast!" ]
                    , p [] [ text "Help him lose weight by taking his diet pills, and ensure that he lives a long and prosperous life." ]
                    , arrowIcons
                    , text "Move around using the arrow keys."
                    , br [] []
                    ]
                , viewPressEnterTo "start the diet!"
                ]

        Paused ->
            div [ class "modal" ]
                [ div [ class "modal-title" ] [ text "- Paused -" ]
                , viewPressEnterTo "resume your diet."
                ]

        GameOver ->
            div [ class "modal" ]
                [ div [ class "modal-title red-text" ] [ text "- Snake is dead -" ]
                , viewPressEnterTo "start a new diet."
                ]

        _ ->
            span [] []


viewGithub : Html Msg
viewGithub =
    let
        url =
            "https://github.com/joelchelliah/diet-snake"
    in
    div
        [ class "github" ]
        [ githubIcon, a [ href url ] [ text "Find me on Github" ] ]


view : Model -> Html Msg
view model =
    div [ class "game" ]
        [ iconCss
        , viewTitle model.state
        , viewMap model
        , viewScoreBoard model
        , viewModal model
        , viewGithub
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
                [ Time.every 100 (\_ -> Tick)
                , Time.every 300 (\_ -> Grow)
                , keyDownSubscription
                ]

        _ ->
            keyDownSubscription


main : Program () Model Msg
main =
    Browser.element
        { init = init initStats
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
