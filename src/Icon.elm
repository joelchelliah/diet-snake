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


topCorners : List String
topCorners =
    [ "up-left", "up-right" ]


bottomCorners : List String
bottomCorners =
    [ "down-left", "down-right" ]


allCorners : List String
allCorners =
    List.append topCorners bottomCorners


repeatIconPerCorner : Icon -> List String -> Html msg
repeatIconPerCorner icon corners =
    let
        html =
            icon |> Icon.present |> Icon.styled [ Icon.sm ] |> Icon.view
    in
    span [ class "corner-icons" ] (List.map (\name -> div [ class name ] [ html ]) corners)


iconCss : Html msg
iconCss =
    Icon.css


cookieIcons : Html msg
cookieIcons =
    repeatIconPerCorner Icon.cookieBite allCorners


pauseIcons : Html msg
pauseIcons =
    repeatIconPerCorner Icon.pause topCorners


skullIcons : Html msg
skullIcons =
    repeatIconPerCorner Icon.skull topCorners


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
    Icon.github |> Icon.present |> Icon.styled [ Icon.fa2x, Icon.pullLeft ] |> Icon.view
