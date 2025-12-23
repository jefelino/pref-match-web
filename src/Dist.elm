module Dist exposing (..)
import Dict exposing (Dict)
import Util.MyMaybe as Maybe

type alias Count = Int

type alias Dist a = Dict a Count

empty : Dist a
empty =
    Dict.empty


add : comparable -> Count -> Dist comparable -> Dist comparable
add key n dist =
    dist |>
        Dict.update key
            (Maybe.withDefault 0 
                >> ((+) n) 
                >> Maybe.test (\k -> k > 0))

increment : comparable -> Dist comparable -> Dist comparable
increment key dist =
    add key 1 dist
    

decrement : comparable -> Dist comparable -> Dist comparable
decrement key dist =
    add key (-1) dist


join : Dist comparable -> Dist comparable -> Dist comparable
join dist1 dist2 =
    Dict.foldl add dist1 dist2

count : List comparable -> Dist comparable
count items =
    List.foldl increment empty items

get : comparable -> Dist comparable -> Count
get key dist =
    Maybe.withDefault 0 (Dict.get key dist)