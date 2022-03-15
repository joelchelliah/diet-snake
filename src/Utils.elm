module Utils exposing (..)

import Model exposing (..)


isPillHere : Position -> Maybe Pill -> Bool
isPillHere pos pill =
    case pill of
        Nothing ->
            False

        Just { position } ->
            position == pos


getNonWallPositions : Map -> List Position
getNonWallPositions =
    let
        getFromRow =
            List.filterMap
                (\tile ->
                    case tile of
                        Wall ->
                            Nothing

                        Open pos ->
                            Just pos
                )
    in
    List.foldl (\row acc -> getFromRow row |> List.append acc) []


getFreeTilePositions : List Position -> Map -> List Position
getFreeTilePositions nonFreePositions map =
    let
        nonWallPositions =
            getNonWallPositions map
    in
    List.filter (\pos -> not <| List.member pos nonFreePositions) nonWallPositions


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
