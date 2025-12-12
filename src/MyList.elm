module MyList exposing (..)

find : (a -> Bool) -> List a -> Maybe a
find f list =
    case list of
        [] -> Nothing
        x::xs -> if f x then Just x else find f xs

findMap : (a -> Maybe b) -> List a -> Maybe b
findMap f list =
    case list of
        [] -> Nothing
        x::xs -> case f x of
            Just y -> Just y
            Nothing -> findMap f xs

