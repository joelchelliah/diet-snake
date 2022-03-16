module Components.Tiles exposing (init, view)

import Components.Pill
import Components.Snake
import Html exposing (Attribute, Html, div, span)
import Html.Attributes exposing (class)
import Model exposing (GameState(..), Map, Model, Msg, Pill, Position, Snake, Tile(..))
import String
import Utils.Animation exposing (fadeAway, pulseAndTurn)
import Utils.ListExtra exposing (getIndexInList)


type alias Container msg =
    List (Attribute msg) -> List (Html msg) -> Html msg


makeTile : String -> Container msg -> Html msg
makeTile className innerTile =
    let
        wrapInOuterTile tile =
            div [ class "outer-tile" ] [ tile ]
    in
    case className of
        "" ->
            innerTile [] [] |> wrapInOuterTile

        name ->
            innerTile [ class ("inner-tile " ++ name) ] [] |> wrapInOuterTile


makePill : Model.PillColor -> Float -> Html msg
makePill color rotation =
    pulseAndTurn rotation |> makeTile (String.join " " [ "pill", Components.Pill.toString color ])


fadeAwayDeadTiles : List Position -> Position -> Html msg
fadeAwayDeadTiles tilePositions currentTilePosition =
    -- The current tile position is only used for offsetting the fade delay
    tilePositions |> getIndexInList currentTilePosition |> fadeAway |> makeTile "snake-dead"


placeTile : { head : Html msg, tail : Html msg, dead : List Position -> Position -> Html msg, pill : Model.PillColor -> Float -> Html msg, wall : Html msg, open : Html msg }
placeTile =
    { head = makeTile "snake-head" div
    , tail = makeTile "snake-tail" div
    , dead = fadeAwayDeadTiles
    , pill = makePill
    , wall = makeTile "wall" div
    , open = makeTile "" span
    }


viewTile : Snake -> Maybe Pill -> Bool -> Tile -> Html Msg
viewTile snake pill isGameOver tile =
    case tile of
        Wall ->
            placeTile.wall

        Open pos ->
            if snake.head == pos then
                if isGameOver then
                    placeTile.dead [ pos ] pos

                else
                    placeTile.head

            else if Components.Snake.isTailHere snake pos then
                if isGameOver then
                    placeTile.dead snake.tail pos

                else
                    placeTile.tail

            else if Components.Pill.isHere pos pill then
                case pill of
                    Nothing ->
                        span [] []

                    Just { color, rotation } ->
                        placeTile.pill color rotation

            else if Components.Snake.isTrimmedTailHere snake pos then
                placeTile.dead snake.trimmed pos

            else
                placeTile.open


init : Int -> Int -> Map
init width height =
    let
        initTile ( x, y ) =
            if x == 1 || y == 1 || x == width || y == height then
                Wall

            else
                Open ( x, y )

        initRow y =
            List.map (\x -> initTile ( x, y )) (List.range 1 width)
    in
    List.map initRow (List.range 1 height)


view : Model -> Html Msg
view { snake, pill, map, state } =
    let
        viewRow row =
            div [ class "row" ] (List.map (viewTile snake pill (state == GameOver)) row)
    in
    div [] (List.map viewRow map)
