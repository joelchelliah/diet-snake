module Components.Tiles exposing (init, view)

import Components.Pill as Pill
import Components.Snake as Snake
import Html exposing (Attribute, Html, div, span)
import Html.Attributes exposing (class)
import Model exposing (GameState(..), Map, Model, Msg, Pill, Position, Snake, Tile(..), config)
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


makePillTile : Model.PillColor -> Float -> Html msg
makePillTile color rotation =
    let
        pillColor =
            Pill.toString color
    in
    Tile ("pill " ++ pillColor) |> makeTile (pulseAndTurn rotation)


makeDeadTile : List Position -> Position -> Html msg
makeDeadTile tilePositions currentTilePosition =
    -- The currentTilePosition is only used to offset the fade animation delay
    let
        fadeAwayContainer =
            tilePositions |> getIndexInListOrDefault currentTilePosition 0 |> fadeAway
    in
    Tile "snake dead" |> makeTile fadeAwayContainer


makeDigestTile : Int -> Html msg
makeDigestTile index =
    let
        suffix =
            index + 1 |> String.fromInt

        validSuffixes =
            config.digestBulgeLength |> List.range 1 |> List.map String.fromInt
    in
    if List.member suffix validSuffixes then
        Tile ("snake digest bulge-" ++ suffix) |> makeTile div

    else
        Tile "snake digest" |> makeTile div


placeTile =
    { head = Tile "snake head" |> makeTile div
    , bigHead = Tile "snake head bulge-1" |> makeTile div
    , biggerHead = Tile "snake head bulge-2" |> makeTile div
    , tail = Tile "snake tail" |> makeTile div
    , wall = Tile "wall" |> makeTile div
    , digest = makeDigestTile
    , dead = makeDeadTile
    , pill = makePillTile
    , empty = makeTile span EmptyTile
    }


placeHeadTile : Snake -> Html msg
placeHeadTile snake =
    if snake.digestingProgress < 2 * Snake.getDigestRate snake then
        placeTile.biggerHead

    else if snake.digestingProgress < 3 * Snake.getDigestRate snake then
        placeTile.bigHead

    else
        placeTile.head


placeTailTile : Snake -> Position -> Html a
placeTailTile snake currentTilePosition =
    let
        digestingPositions =
            Snake.getCurrentlyDigestingTailPortion snake
    in
    if List.member currentTilePosition digestingPositions then
        getIndexInListOrDefault currentTilePosition 1337 digestingPositions |> placeTile.digest

    else
        placeTile.tail


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
                    placeHeadTile snake

            else if Snake.isTailHere snake pos then
                if isGameOver then
                    placeTile.dead snake.tail pos

                else
                    placeTailTile snake pos

            else if Pill.isHere pos pill then
                case pill of
                    Nothing ->
                        span [] []

                    Just { color, rotation } ->
                        placeTile.pill color rotation

            else if Snake.isTrimmedTailHere snake pos then
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
