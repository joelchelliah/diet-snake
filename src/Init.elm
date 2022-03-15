module Init exposing (..)

import Config exposing (config)
import Snake
import Stats
import Types exposing (Direction(..), GameState(..), Map, Model, Msg, StatDetails, Tile(..))


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


init : StatDetails -> () -> ( Model, Cmd Msg )
init bestStats () =
    ( { snake = Snake.init 5
      , pill = Nothing
      , state = Init
      , map = initMap config.gameWidth config.gameHeight
      , stats = Stats.init bestStats
      }
    , Cmd.none
    )
