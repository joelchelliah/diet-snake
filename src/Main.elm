module Main exposing (..)

import Animation exposing (..)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Icon exposing (..)
import Model exposing (..)
import Subscription exposing (subscriptions)
import Update exposing (update)
import Utils exposing (..)


viewTile : Snake -> Pill -> Bool -> Tile -> Html Msg
viewTile snake pill isGameOver tile =
    let
        makeTile className innerTile =
            case className of
                "" ->
                    div [ class "outer-tile" ] [ innerTile [] [] ]

                name ->
                    div [ class "outer-tile" ] [ innerTile [ class ("inner-tile " ++ name) ] [] ]

        fadeAwayDeadTiles tiles pos =
            tiles |> getIndexInList pos |> fadeAway |> makeTile "snake-dead"
    in
    case tile of
        Wall ->
            makeTile "wall" div

        Open pos ->
            if snake.head == pos then
                if isGameOver then
                    fadeAwayDeadTiles [ pos ] pos

                else
                    makeTile "snake-head" div

            else if isSnakeHere snake pos then
                if isGameOver then
                    fadeAwayDeadTiles snake.tail pos

                else
                    makeTile "snake-body" div

            else if isPillHere pill pos then
                makeTile "pill" pulse

            else if isTrimmedAwaySnakeHere snake pos then
                fadeAwayDeadTiles snake.trimmed pos

            else
                makeTile "" span


viewMap : Model -> Html Msg
viewMap { snake, pill, map, state } =
    let
        viewRow row =
            div [ class "row" ] (List.map (viewTile snake pill (state == GameOver)) row)
    in
    div [] (List.map viewRow map)


viewHeader : GameState -> Html Msg
viewHeader state =
    let
        subTitleText =
            if state == GameOver then
                text "Whoops! ..Maybe try another diet?"

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
        [ div [ class "icon" ] [ text "🐍" ]
        , div [ class "titles" ]
            [ div [ class "title" ] [ titleText ]
            , div [ class "subtitle" ] [ subTitleText ]
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
            growAppear 2.4
                [ class "modal" ]
                [ cookieIcons
                , div [ class "title" ] [ text "- Oh no -" ]
                , div [ class "text" ]
                    [ p [] [ text "Mr. Snake is growing too fast!" ]
                    , p [] [ text "Help him lose weight by taking his diet pills, and ensure that he lives a long and prosperous life." ]
                    , arrowIcons
                    , text "Move around using the arrow keys."
                    ]
                , viewPressEnterTo "start the diet!"
                ]

        Paused ->
            growAppear 1.2
                [ class "modal" ]
                [ pauseIcons
                , div [ class "title" ] [ text "- Paused -" ]
                , viewPressEnterTo "resume your diet."
                ]

        GameOver ->
            growAppear 2.0
                [ class "modal" ]
                [ skullIcons
                , div [ class "title red-text" ] [ text "- Snake is dead -" ]
                , viewPressEnterTo "start a new diet."
                ]

        _ ->
            span [] []


viewScoreBoards : Model -> Html Msg
viewScoreBoards { stats, bestStats } =
    let
        viewScore ( key, val, postfix ) =
            div [ class "score" ] [ div [] [ text (key ++ ":") ], div [] [ text (String.fromInt val ++ " " ++ postfix) ] ]

        viewScores scores =
            List.map viewScore scores
    in
    div [ class "scoreboards" ]
        [ div [ class "scoreboard", style "align-items" "flex-start" ]
            (viewScores
                [ ( "Distance covered", stats.stepsTaken, "cm" )
                , ( "Pills taken", stats.pillsTaken, "mg" )
                , ( "Weight lost", stats.weightLoss, "kg" )
                ]
            )
        , div [ class "scoreboard-divider" ] []
        , div [ class "scoreboard", style "align-items" "flex-end" ]
            (viewScores
                [ ( "Longest distance", bestStats.stepsTaken, "cm " )
                , ( "Most pills taken", bestStats.pillsTaken, "mg" )
                , ( "Maximum weightloss", bestStats.weightLoss, "kg" )
                ]
            )
        ]


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
        , viewHeader model.state
        , viewMap model
        , viewScoreBoards model
        , viewModal model
        , viewGithub
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init initStats
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
