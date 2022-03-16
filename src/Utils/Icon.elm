module Utils.Icon exposing (..)

import FontAwesome.Attributes as Icon
import FontAwesome.Brands as Icon
import FontAwesome.Icon as Icon exposing (Icon)
import FontAwesome.Layering as Icon
import FontAwesome.Solid as Icon
import FontAwesome.Styles as Icon
import FontAwesome.Transforms as Icon
import Html exposing (..)
import Html.Attributes exposing (..)


topCornerClasses : List String
topCornerClasses =
    [ "up-left", "up-right" ]


bottomCornerClasses : List String
bottomCornerClasses =
    [ "down-left", "down-right" ]


allCorners : List String
allCorners =
    List.append topCornerClasses bottomCornerClasses


type CornerIcon
    = Cookie
    | Pause
    | Skull


placeIconInCorners : Icon -> List String -> Html msg
placeIconInCorners icon corners =
    let
        html =
            icon |> Icon.present |> Icon.styled [ Icon.sm ] |> Icon.view
    in
    span [ class "corner-icons" ] (List.map (\name -> div [ class name ] [ html ]) corners)


iconCss : Html msg
iconCss =
    Icon.css


viewCornerIcons : CornerIcon -> Html msg
viewCornerIcons icon =
    case icon of
        Cookie ->
            placeIconInCorners Icon.cookieBite allCorners

        Pause ->
            placeIconInCorners Icon.pause topCornerClasses

        Skull ->
            placeIconInCorners Icon.skull topCornerClasses


viewArrowIcons : Html msg
viewArrowIcons =
    let
        viewIcon i =
            i |> Icon.present |> Icon.styled [ Icon.lg ] |> Icon.view

        icons =
            [ Icon.arrowCircleUp, Icon.arrowCircleRight, Icon.arrowCircleDown, Icon.arrowCircleLeft ]
    in
    div [ class "arrow-icons" ]
        (List.map (\i -> span [ class "arrow-icon" ] [ viewIcon i ]) icons)


viewArrowUpIcon : Html msg
viewArrowUpIcon =
    Icon.caretUp |> Icon.present |> Icon.styled [ Icon.lg ] |> Icon.view


viewGithubIcon : Html msg
viewGithubIcon =
    Icon.github |> Icon.present |> Icon.styled [ Icon.fa2x, Icon.pullLeft ] |> Icon.view
