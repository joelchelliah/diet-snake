module Types exposing (..)


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
    }


type alias Pill =
    { position : Position
    , color : String
    , rotation : Float
    }


type alias Map =
    List Row


type alias Row =
    List Tile


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
