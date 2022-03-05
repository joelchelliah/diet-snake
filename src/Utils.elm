module Utils exposing (..)

import Model exposing (..)


isFoodHere : Food -> Position -> Bool
isFoodHere food pos =
    case food of
        Nothing ->
            False

        Just foodPos ->
            foodPos == pos


isSnakeHere : Snake -> Position -> Bool
isSnakeHere { head, tail } pos =
    List.any (\sPos -> sPos == pos) (head :: tail)


isSnakeOnFood : Snake -> Food -> Bool
isSnakeOnFood { head } food =
    isFoodHere food head
