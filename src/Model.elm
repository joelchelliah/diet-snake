module Model exposing (..)


type alias Model =
    { snake : Snake
    , food : Food
    , state : GameState
    , map : Map
    , score : Int
    , highscore : Int
    }


type alias Position =
    ( Int, Int )


type alias Snake =
    { head : Position
    , tail : List Position
    , isGrowing : Bool
    }


type alias Food =
    Maybe Position


type Direction
    = Up
    | Down
    | Left
    | Right


type GameState
    = Moving Direction
    | GameOver


type alias Map =
    List Row


type alias Row =
    List Tile


type Tile
    = Wall
    | Open Position


type Msg
    = StartGame
    | KeyPress Direction
    | Tick
    | NewFood Position


createSnake : Int -> Snake
createSnake maxLength =
    let
        head =
            ( gameWidth // 2, gameHeight // 2 )

        createTail length =
            if length == maxLength then
                []

            else
                Tuple.mapSecond (\y -> y + length) head :: createTail (length + 1)
    in
    { head = head
    , tail = createTail 0
    , isGrowing = False
    }


createTile : Int -> Int -> Position -> Tile
createTile width height ( x, y ) =
    if x == 1 || y == 1 || x == width || y == height then
        Wall

    else
        Open ( x, y )


createRow : Int -> Int -> Int -> Row
createRow width height y =
    List.map (\x -> createTile width height ( x, y )) (List.range 1 width)


createMap : Int -> Int -> Map
createMap width height =
    List.map (createRow width height) (List.range 1 height)


init : Int -> () -> ( Model, Cmd Msg )
init highscore () =
    ( { snake = createSnake 5
      , food = Nothing
      , state = Moving Up
      , map = createMap gameWidth gameHeight
      , score = 0
      , highscore = highscore
      }
    , Cmd.none
    )


gameWidth : number
gameWidth =
    40


gameHeight : number
gameHeight =
    30
