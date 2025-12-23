module Util.MyDict exposing (..)

import Dict exposing (Dict)

pop : Dict comparable v -> Maybe (comparable, v, Dict comparable v)
pop dict =
    case Dict.toList dict of
        [] ->
            Nothing

        (key, value) :: _ ->
            Just ( key, value, Dict.remove key dict )

