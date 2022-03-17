module Model exposing (..)


type alias StatDetails =
    { weightLoss : Int
    , pillsTaken : Int
    , stepsTaken : Int
    }


type alias Stats =
    { current : StatDetails
    , best : StatDetails
    , prevBest : StatDetails
    }


type alias Model =
    { snake : Snake
    , pill : Maybe Pill
    , state : GameState
    , map : Map
    , stats : Stats
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
    , digestingProgress : Int -- Index of first digesting tile
    }


type alias Pill =
    { position : Position
    , color : PillColor
    , rotation : Float
    }


type alias Map =
    List Row


type alias Row =
    List Tile


type PillColor
    = Green
    | Blue
    | Yellow
    | Pink
    | Teal


type Tile
    = Wall
    | Open Position


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


type Msg
    = StartGame
    | KeyPress Direction
    | Enter
    | Tick
    | Grow
    | NewPill Pill
    | Trim Int


config =
    { gameWidth = 32
    , gameHeight = 24
    , gameSpeed = 90 -- Lower number -> faster
    , growthStartAt = 10 -- Number of steps
    , growthRate = 180 -- Number of milliseconds between
    , digestRate = 3 -- Number of tail tiles to step through each time
    , digestBulgeLength = 5 -- Number of tail tiles to bulge
    }
