module Utils.ListExtra exposing (..)


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
