module Update exposing (update)

import Model exposing (..)
import Random
import Utils exposing (..)


getNonWallPositions : Map -> List Position
getNonWallPositions =
    let
        getFromRow =
            List.filterMap
                (\tile ->
                    case tile of
                        Wall ->
                            Nothing

                        Open pos ->
                            Just pos
                )
    in
    List.foldl (\row acc -> getFromRow row |> List.append acc) []


getFreeTilePositions : List Position -> Map -> List Position
getFreeTilePositions snakePositions map =
    let
        nonWallPositions =
            getNonWallPositions map
    in
    List.filter (\pos -> not <| List.member pos snakePositions) nonWallPositions


isSnakeAlive : Snake -> Map -> Bool
isSnakeAlive { head, tail } map =
    let
        isHeadOn pos =
            head == pos
    in
    getFreeTilePositions tail map |> List.any isHeadOn


updateSnake : Snake -> Food -> GameState -> Snake
updateSnake snake food state =
    let
        { head, tail, isGrowing } =
            snake

        newTail =
            if isGrowing then
                head :: tail

            else
                head :: List.take (List.length tail - 1) tail

        updateFields newHead =
            { snake
                | head = newHead
                , tail = newTail
                , isGrowing = isFoodHere food newHead
            }
    in
    case snake.direction of
        Up ->
            Tuple.mapSecond (\v -> v - 1) head |> updateFields

        Down ->
            Tuple.mapSecond (\v -> v + 1) head |> updateFields

        Left ->
            Tuple.mapFirst (\v -> v - 1) head |> updateFields

        Right ->
            Tuple.mapFirst (\v -> v + 1) head |> updateFields


positionGenerator : Snake -> Map -> Random.Generator Position
positionGenerator { head, tail } map =
    let
        freePositions =
            getFreeTilePositions (head :: tail) map

        indexGenerator =
            Random.int 0 (List.length freePositions - 1)

        lookUpPosition i =
            case List.drop i freePositions |> List.head of
                -- Never occurs since lookUp is always successful
                Nothing ->
                    ( 1, 1 )

                Just pos ->
                    pos
    in
    Random.map lookUpPosition indexGenerator


turnSnake : Snake -> Direction -> Snake
turnSnake snake direction =
    let
        currentDirection =
            snake.direction

        isValid =
            ((direction == Up || direction == Down) && (currentDirection == Left || currentDirection == Right))
                || ((direction == Left || direction == Right) && (currentDirection == Up || currentDirection == Down))

        newDirection =
            if isValid then
                direction

            else
                currentDirection
    in
    { snake | direction = newDirection }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        isPaused =
            model.state == Paused
    in
    case msg of
        StartGame ->
            let
                { stats, bestStats } =
                    model

                newWeightLoss =
                    if stats.weightLoss > bestStats.weightLoss then
                        stats.weightLoss

                    else
                        bestStats.weightLoss
            in
            init { bestStats | weightLoss = newWeightLoss } ()

        Pause ->
            if isPaused then
                ( { model | state = Running }, Cmd.none )

            else
                ( { model | state = Paused }, Cmd.none )

        KeyPress direction ->
            ( { model | snake = turnSnake model.snake direction }, Cmd.none )

        Tick ->
            let
                { snake, state, food, map } =
                    model

                newSnake =
                    updateSnake snake food state

                newCommand =
                    if food == Nothing || isSnakeOnFood newSnake food then
                        Random.generate NewFood (positionGenerator newSnake map)

                    else
                        Cmd.none
            in
            if isPaused then
                ( model, Cmd.none )

            else if isSnakeAlive newSnake map then
                ( { model | snake = newSnake }, newCommand )

            else
                ( { model | state = GameOver }, Cmd.none )

        NewFood pos ->
            ( { model | food = Just pos }, Cmd.none )
