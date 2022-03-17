module Utils.List exposing (..)


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


getIndexInListOrDefault : a -> Int -> List a -> Int
getIndexInListOrDefault a default list =
    case list of
        head :: tail ->
            if head == a then
                0

            else
                1 + getIndexInListOrDefault a default tail

        _ ->
            default
