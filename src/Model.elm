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
    , frameDeltas : FrameDeltaCounters
    }


type alias FrameDeltaCounters =
    -- To calculate correct time intervals according to browser's render loop
    { tickDelta : Float
    , growDelta : Float
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
    , metabolism : Metabolism
    }


type alias Metabolism =
    { progress : Int -- Index of the first bulging tile
    , bulgeRotation : BulgeRotation -- Rotate bulge left or right for CSS animation
    , isActive : Bool
    , rate : Int
    }


type BulgeRotation
    = Clockwise
    | CounterClockwise


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
    | FrameDelta Float


config =
    { gameWidth = 32
    , gameHeight = 24
    , gameSpeed = 90 -- Number of milliseconds between each Tick
    , growthStartAt = 10 -- Number of steps before growth can start
    , growthRate = 180 -- Number of milliseconds between each growth spurt
    }
