module Main exposing (..)

import Animation exposing (..)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Icon exposing (..)
import Model exposing (..)
import String exposing (join)
import Subscription exposing (subscriptions)
import Update exposing (update)
import Utils exposing (..)


viewTile : Snake -> Maybe Pill -> Bool -> Tile -> Html Msg
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

            else if isPillHere pos pill then
                case pill of
                    Nothing ->
                        span [] []

                    Just { color, rotation } ->
                        pulseAndTurn rotation |> makeTile (join " " [ "pill", color ])

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

        subTitleText =
            if state == GameOver then
                text "Whoops! ...Try another diet?"

            else
                text "The totally backward snake game!"
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
                [ viewCornerIcons Cookie
                , div [ class "title" ] [ text "- Oh no -" ]
                , div [ class "text" ]
                    [ p [] [ text "Mr. Snake is growing too fast!" ]
                    , p [] [ text "Help him lose weight by taking his diet pills, and ensure that he lives a long and prosperous life." ]
                    , viewArrowIcons
                    , text "Move around using the arrow keys."
                    ]
                , viewPressEnterTo "start the diet!"
                ]

        Paused ->
            growAppear 1.2
                [ class "modal" ]
                [ viewCornerIcons Pause
                , div [ class "title" ] [ text "- Paused -" ]
                , viewPressEnterTo "resume your diet."
                ]

        GameOver ->
            growAppear 2.0
                [ class "modal" ]
                [ viewCornerIcons Skull
                , div [ class "title red" ] [ text "- Snake is dead -" ]
                , viewPressEnterTo "start a new diet."
                ]

        _ ->
            span [] []


viewScore : String -> Int -> String -> Int -> Bool -> Html msg
viewScore key val postfix best arrowFirst =
    let
        arrow =
            if val > best then
                fadeAndRise [ class "arrow-up highlight" ] [ viewArrowUpIcon ]

            else
                div [ class "arrow-up" ] []

        score =
            div
                [ class
                    (if val > best then
                        "score highlight"

                     else
                        "score"
                    )
                ]
                [ div [] [ text (key ++ ":") ]
                , div [] [ text (String.fromInt val ++ " " ++ postfix) ]
                ]
    in
    div [ class "score-and-arrow" ]
        (if arrowFirst then
            [ arrow, score ]

         else
            [ score, arrow ]
        )


viewScoreBoards : Stats -> Html Msg
viewScoreBoards { current, best, prevBest } =
    div [ class "scoreboards" ]
        [ div [ class "scoreboard", style "align-items" "flex-start" ]
            [ viewScore "Distance covered" current.stepsTaken "cm" best.stepsTaken False
            , viewScore "Pills taken" current.pillsTaken "mg" best.pillsTaken False
            , viewScore "Weight lost" current.weightLoss "kg" best.weightLoss False
            ]
        , div [ class "scoreboard-divider" ] []
        , div [ class "scoreboard", style "align-items" "flex-end" ]
            [ viewScore "Longest distance" best.stepsTaken "cm " prevBest.stepsTaken True
            , viewScore "Most pills taken" best.pillsTaken "mg" prevBest.pillsTaken True
            , viewScore "Maximum weightloss" best.weightLoss "kg" prevBest.weightLoss True
            ]
        ]


viewGithub : Html Msg
viewGithub =
    let
        url =
            "https://github.com/joelchelliah/diet-snake"
    in
    div
        [ class "github" ]
        [ viewGithubIcon, a [ href url ] [ text "Find me on Github" ] ]


view : Model -> Html Msg
view model =
    div [ class "game" ]
        [ iconCss
        , viewHeader model.state
        , viewMap model
        , viewScoreBoards model.stats
        , viewModal model
        , viewGithub
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init initStatDetails
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
