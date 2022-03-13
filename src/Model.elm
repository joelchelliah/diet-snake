module Model exposing (..)


type alias Model =
    { snake : Snake
    , pill : Pill
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
    , trimmed : List Position
    , direction : Direction
    , isGrowing : Bool
    , canGrow : Bool -- To prevent snake from growing immediately
    }


type alias Stats =
    { weightLoss : Int
    , pillsTaken : Int
    , stepsTaken : Int
    }


type alias Pill =
    Maybe Position


type Direction
    = Up
    | Down
    | Left
    | Right


type GameState
    = Running
    | Paused
    | Init
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
    | Enter
    | Tick
    | Grow
    | NewPill Position
    | Trim Int


config : { gameWidth : number, gameHeight : number, gameSpeed : number, growthStartAt : number, growthRate : number }
config =
    { gameWidth = 32
    , gameHeight = 24
    , gameSpeed = 90 -- Lower number -> faster
    , growthStartAt = 10
    , growthRate = 180
    }


initSnake : Int -> Snake
initSnake maxLength =
    let
        head =
            ( config.gameWidth // 2, config.gameHeight // 2 )

        createTail length =
            if length == maxLength then
                []

            else
                Tuple.mapSecond (\y -> y + length) head :: createTail (length + 1)
    in
    { head = head
    , tail = createTail 0
    , trimmed = []
    , direction = Up
    , isGrowing = False
    , canGrow = False
    }


initMap : Int -> Int -> Map
initMap width height =
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


initStats : Stats
initStats =
    { weightLoss = 0
    , pillsTaken = 0
    , stepsTaken = 0
    }


init : Stats -> () -> ( Model, Cmd Msg )
init bestStats () =
    ( { snake = initSnake 5
      , pill = Nothing
      , state = Init
      , map = initMap config.gameWidth config.gameHeight
      , stats = initStats
      , bestStats = bestStats
      }
    , Cmd.none
    )
