module Icon exposing (..)

import FontAwesome.Attributes as Icon
import FontAwesome.Brands as Icon
import FontAwesome.Icon as Icon exposing (Icon)
import FontAwesome.Layering as Icon
import FontAwesome.Solid as Icon
import FontAwesome.Styles as Icon
import FontAwesome.Svg as SvgIcon
import FontAwesome.Transforms as Icon
import Html exposing (..)


iconCss : Html msg
iconCss =
    Icon.css


githubIcon : Html msg
githubIcon =
    Icon.github |> Icon.present |> Icon.styled [ Icon.lg, Icon.pullLeft ] |> Icon.view


arrowIcons : List (Html msg)
arrowIcons =
    let
        viewIcon i =
            i |> Icon.present |> Icon.transform [ Icon.shrink 2 ] |> Icon.styled [ Icon.lg ] |> Icon.view
    in
    List.map viewIcon
        [ Icon.arrowCircleUp, Icon.arrowCircleRight, Icon.arrowCircleDown, Icon.arrowCircleLeft ]
