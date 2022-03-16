module Components.Hud exposing (..)

import Html exposing (Html, a, b, div, i, p, span, text)
import Html.Attributes exposing (class, href)
import Model exposing (GameState(..), Model, Msg)
import Utils.Animation exposing (growAppear)
import Utils.Icon exposing (CornerIcon(..), viewArrowIcons, viewCornerIcons, viewGithubIcon)


viewPressEnterTo : String -> Html Msg
viewPressEnterTo reason =
    div []
        [ text "Press "
        , b [] [ i [ class "enter" ] [ text "Enter" ] ]
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


viewHeader : Model -> Html Msg
viewHeader { state } =
    let
        title =
            if state == GameOver then
                div [ class "title" ]
                    [ span [ class "red" ] [ text "D" ]
                    , span [ class "cancelled" ] [ text "iet" ]
                    , span [ class "red" ] [ text "ead " ]
                    , text "Snake"
                    ]

            else
                div [ class "title" ] [ text "Diet Snake" ]

        subTitle =
            if state == GameOver then
                div [ class "subtitle" ] [ text "Whoops! ...Try another diet?" ]

            else
                div [ class "subtitle" ] [ text "The totally backward snake game!" ]
    in
    div [ class "header" ]
        [ div [ class "icon" ] [ text "🐍" ]
        , div [ class "title-container" ] [ title, subTitle ]
        ]


viewFooter : Html Msg
viewFooter =
    div
        [ class "github" ]
        [ viewGithubIcon
        , a [ href "https://github.com/joelchelliah/diet-snake" ]
            [ text "Find me on Github" ]
        ]
