module Components.Tiles exposing (init, view)

import Components.Pill
import Components.Snake
import Html exposing (Attribute, Html, div, span)
import Html.Attributes exposing (class)
import Model exposing (GameState(..), Map, Model, Msg, Pill, Position, Snake, Tile(..))
import Utils.Animation exposing (fadeAway, pulseAndTurn)
import Utils.ListExtra exposing (getIndexInList)


type alias Container msg =
    List (Attribute msg) -> List (Html msg) -> Html msg


type TileClass
    = EmptyTile
    | Tile String


makeTile : Container msg -> TileClass -> Html msg
makeTile tileContainer tileClass =
    let
        wrapInBackgroundTile tile =
            div [ class "background-tile" ] [ tile ]
    in
    case tileClass of
        EmptyTile ->
            tileContainer [] [] |> wrapInBackgroundTile

        Tile name ->
            tileContainer [ class ("tile " ++ name) ] [] |> wrapInBackgroundTile


makePill : Model.PillColor -> Float -> Html msg
makePill color rotation =
    let
        pillColor =
            Components.Pill.toString color
    in
    Tile ("pill " ++ pillColor) |> makeTile (pulseAndTurn rotation)


makeDeadTile : List Position -> Position -> Html msg
makeDeadTile tilePositions currentTilePosition =
    -- The currentTilePosition is only used to offset the fade animation delay
    let
        fadeAwayContainer =
            tilePositions |> getIndexInList currentTilePosition |> fadeAway
    in
    Tile "snake dead" |> makeTile fadeAwayContainer


placeTile : { head : Html msg, tail : Html msg, dead : List Position -> Position -> Html msg, pill : Model.PillColor -> Float -> Html msg, wall : Html msg, empty : Html msg }
placeTile =
    { head = Tile "snake head" |> makeTile div
    , tail = Tile "snake tail" |> makeTile div
    , wall = Tile "wall" |> makeTile div
    , dead = makeDeadTile
    , pill = makePill
    , empty = makeTile span EmptyTile
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
                placeTile.empty


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
