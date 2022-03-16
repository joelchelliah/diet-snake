module Utils.Position exposing (..)

import Model exposing (Map, Position, Tile(..))


getNonWallTilePositions : Map -> List Position
getNonWallTilePositions =
    let
        getNonWallPositionsFromRow =
            List.filterMap
                (\tile ->
                    case tile of
                        Wall ->
                            Nothing

                        Open pos ->
                            Just pos
                )
    in
    List.foldl (\row acc -> getNonWallPositionsFromRow row |> List.append acc) []


getFreeTilePositions : List Position -> Map -> List Position
getFreeTilePositions nonFreePositions map =
    getNonWallTilePositions map
        |> List.filter (\pos -> List.member pos nonFreePositions |> not)
