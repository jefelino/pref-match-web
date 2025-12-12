module Shuffle exposing (shuffle)

{-| Fisher-Yates shuffle implementation for randomizing lists

@docs shuffle

-}

import Array
import Random


{-| Shuffle a list using the Fisher-Yates algorithm

Returns a Random generator that produces a shuffled version of the input list.
-}
shuffle : List a -> Random.Generator (List a)
shuffle list =
    let
        array = Array.fromList list
        n = Array.length array
    in
    shuffleArray array 0 n
        |> Random.map Array.toList


{-| Fisher-Yates shuffle implementation for arrays -}
shuffleArray : Array.Array a -> Int -> Int -> Random.Generator (Array.Array a)
shuffleArray array currentIndex length =
    if currentIndex >= length - 1 then
        Random.constant array
    else
        Random.int currentIndex (length - 1)
            |> Random.andThen
                (\randomIndex ->
                    let
                        swapped = swapArrayElements currentIndex randomIndex array
                    in
                    shuffleArray swapped (currentIndex + 1) length
                )


{-| Swap two elements in an array -}
swapArrayElements : Int -> Int -> Array.Array a -> Array.Array a
swapArrayElements i j array =
    let
        maybeI = Array.get i array
        maybeJ = Array.get j array
    in
    case (maybeI, maybeJ) of
        (Just valI, Just valJ) ->
            array
                |> Array.set i valJ
                |> Array.set j valI
        
        _ ->
            array
