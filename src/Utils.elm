module Utils exposing (..)

import Types exposing (Map, Position, Tile(..))


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


lookUpInListOrDefault : List a -> a -> Int -> a
lookUpInListOrDefault list default i =
    let
        found =
            List.drop i list |> List.head
    in
    case found of
        Nothing ->
            default

        Just value ->
            value


getIndexInList : a -> List a -> Int
getIndexInList a list =
    case list of
        head :: tail ->
            if head == a then
                0

            else
                1 + getIndexInList a tail

        _ ->
            0
