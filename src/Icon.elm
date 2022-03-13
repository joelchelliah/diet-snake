module Icon exposing (..)

import FontAwesome.Attributes as Icon
import FontAwesome.Brands as Icon
import FontAwesome.Icon as Icon exposing (Icon)
import FontAwesome.Layering as Icon
import FontAwesome.Solid as Icon
import FontAwesome.Styles as Icon
import FontAwesome.Transforms as Icon
import Html exposing (..)
import Html.Attributes exposing (..)


allCorners : List String
allCorners =
    [ "icon-up-left", "icon-up-right", "icon-down-left", "icon-down-right" ]


onlyTopCorners : List String
onlyTopCorners =
    [ "icon-up-left", "icon-up-right" ]


repeatIconPerClass : Icon -> List String -> Html msg
repeatIconPerClass icon classes =
    let
        html =
            icon |> Icon.present |> Icon.styled [ Icon.sm ] |> Icon.view
    in
    span [] (List.map (\name -> div [ class name ] [ html ]) classes)


iconCss : Html msg
iconCss =
    Icon.css


cookieIcons : Html msg
cookieIcons =
    repeatIconPerClass Icon.cookieBite allCorners


pauseIcons : Html msg
pauseIcons =
    repeatIconPerClass Icon.pause onlyTopCorners


skullIcons : Html msg
skullIcons =
    repeatIconPerClass Icon.skull onlyTopCorners


arrowIcons : Html msg
arrowIcons =
    let
        viewIcon i =
            i |> Icon.present |> Icon.styled [ Icon.lg ] |> Icon.view
    in
    div [ class "arrow-icons" ]
        (List.map
            (\i -> span [ class "arrow-icon" ] [ viewIcon i ])
            [ Icon.arrowCircleUp, Icon.arrowCircleRight, Icon.arrowCircleDown, Icon.arrowCircleLeft ]
        )


githubIcon : Html msg
githubIcon =
    Icon.githubSquare |> Icon.present |> Icon.styled [ Icon.lg, Icon.pullLeft ] |> Icon.view
