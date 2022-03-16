module Init exposing (init)

import Components.Snake
import Components.Stats
import Components.Tiles
import Config exposing (config)
import Types exposing (Direction(..), GameState(..), Model, Msg, StatDetails, Tile(..))


init : StatDetails -> () -> ( Model, Cmd Msg )
init bestStats () =
    ( { snake = Components.Snake.init 5
      , pill = Nothing
      , state = Init
      , map = Components.Tiles.init config.gameWidth config.gameHeight
      , stats = Components.Stats.init bestStats
      }
    , Cmd.none
    )
