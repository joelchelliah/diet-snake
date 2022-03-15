module Init exposing (init)

import Config exposing (config)
import Snake
import Stats
import Tiles
import Types exposing (Direction(..), GameState(..), Model, Msg, StatDetails, Tile(..))


init : StatDetails -> () -> ( Model, Cmd Msg )
init bestStats () =
    ( { snake = Snake.init 5
      , pill = Nothing
      , state = Init
      , map = Tiles.init config.gameWidth config.gameHeight
      , stats = Stats.init bestStats
      }
    , Cmd.none
    )
