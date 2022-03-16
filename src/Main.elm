module Main exposing (..)

import Browser
import Components.Stats
import Components.Tiles
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Init exposing (init)
import Subscription exposing (subscriptions)
import Types exposing (GameState(..), Model, Msg, Tile(..))
import Update exposing (update)
import Utils.Animation exposing (growAppear)
import Utils.Icon exposing (CornerIcon(..), iconCss, viewArrowIcons, viewCornerIcons, viewGithubIcon)
import Utils.ListExtra exposing (..)


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
        , Components.Tiles.view model
        , Components.Stats.view model.stats
        , viewModal model
        , viewGithub
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init Components.Stats.initDetails
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
