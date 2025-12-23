module Util.MyMaybe exposing (..)
import Util.Order as Order

test : (a -> Bool) -> a -> Maybe a
test predicate value =
    if predicate value then
        Just value
    else
        Nothing
join : List (Maybe a) -> Maybe (List a)
join maybes =
    List.foldr (Maybe.map2 (::) ) (Just []) maybes

compare : Maybe comparable -> Maybe comparable -> Order
compare maybeA maybeB =
    case (maybeA, maybeB) of
        (Nothing, Nothing) ->
            EQ

        (Just _, Nothing) ->
            GT

        (Nothing, Just _) ->
            LT

        (Just a, Just b) ->
            Basics.compare a b

min : Maybe comparable -> Maybe comparable -> Maybe comparable
min = Order.minWith compare

max : Maybe comparable -> Maybe comparable -> Maybe comparable
max = Order.maxWith compare