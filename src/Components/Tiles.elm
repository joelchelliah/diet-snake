module Components.Tiles exposing (init, view)

import Components.Metabolism as Metabolism
import Components.Pill as Pill
import Components.Snake as Snake
import Html exposing (Attribute, Html, div, span)
import Html.Attributes exposing (class)
import Model exposing (BulgeRotation(..), GameState(..), Map, Model, Msg, Pill, Position, Snake, Tile(..), config)
import Utils.Animation exposing (fadeAway, pulseAndTurn)
import Utils.List exposing (getIndexInListOrDefault)


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


placeWallTile : Html msg
placeWallTile =
    Tile "wall" |> makeTile div


placeEmptyTile : Html msg
placeEmptyTile =
    makeTile span EmptyTile


placeHeadTile : Snake -> Html msg
placeHeadTile { metabolism } =
    Tile
        (Metabolism.getHeadBulgeClass "snake head" metabolism)
        |> makeTile div


placeTailTile : Snake -> Position -> Html a
placeTailTile snake currentTilePosition =
    let
        baseClass =
            "snake tail"

        digestingPositions =
            Snake.getCurrentlyDigestingTailPortion snake

        index =
            getIndexInListOrDefault currentTilePosition 1337 digestingPositions
    in
    if index /= 1337 then
        Tile
            (Metabolism.getTailBulgeClass index baseClass snake.metabolism)
            |> makeTile div

    else
        Tile baseClass |> makeTile div


placeDeadTile : List Position -> Position -> Html msg
placeDeadTile tilePositions currentTilePosition =
    -- The currentTilePosition is only used to offset the fade animation delay
    let
        fadeAwayContainer =
            tilePositions |> getIndexInListOrDefault currentTilePosition 0 |> fadeAway
    in
    Tile "snake dead" |> makeTile fadeAwayContainer


placePillTile : Model.PillColor -> Float -> Html msg
placePillTile color rotation =
    let
        pillColor =
            Pill.toString color
    in
    Tile ("pill " ++ pillColor) |> makeTile (pulseAndTurn rotation)


viewTile : Snake -> Maybe Pill -> Bool -> Tile -> Html Msg
viewTile snake pill isGameOver tile =
    case tile of
        Wall ->
            placeWallTile

        Open pos ->
            if snake.head == pos then
                if isGameOver then
                    placeDeadTile [ pos ] pos

                else
                    placeHeadTile snake

            else if Snake.isTailHere snake pos then
                if isGameOver then
                    placeDeadTile snake.tail pos

                else
                    placeTailTile snake pos

            else if Pill.isHere pos pill then
                case pill of
                    Nothing ->
                        span [] []

                    Just { color, rotation } ->
                        placePillTile color rotation

            else if Snake.isTrimmedTailHere snake pos then
                placeDeadTile snake.trimmed pos

            else
                placeEmptyTile


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
