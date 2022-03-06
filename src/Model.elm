module Model exposing (..)


type alias Model =
    { snake : Snake
    , food : Food
    , state : GameState
    , map : Map
    , stats : Stats
    , bestStats : Stats
    }


type alias Position =
    ( Int, Int )


type alias Snake =
    { head : Position
    , tail : List Position
    , isGrowing : Bool
    , direction : Direction
    }


type alias Stats =
    { pillsTaken : Int
    , weightLoss : Int
    }


type alias Food =
    Maybe Position


type Direction
    = Up
    | Down
    | Left
    | Right


type GameState
    = Running
    | Paused
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
    | Pause
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
    , direction = Up
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


initStats : Stats
initStats =
    { weightLoss = 0
    , pillsTaken = 0
    }


init : Stats -> () -> ( Model, Cmd Msg )
init bestStats () =
    ( { snake = createSnake 5
      , food = Nothing
      , state = Running
      , map = createMap gameWidth gameHeight
      , stats = initStats
      , bestStats = initStats
      }
    , Cmd.none
    )


gameWidth : number
gameWidth =
    40


gameHeight : number
gameHeight =
    30
