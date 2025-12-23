module Util.MyList exposing (..)

import Util.MyMaybe as Maybe
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

maximumBy : (a -> comparable) -> List a -> Maybe a
maximumBy keyFunc list =
    let helper currentKey currentVal xs =
            case xs of
                [] ->
                    currentVal
                
                y::ys ->
                    let
                        key = keyFunc y 
                    in
                    if Maybe.compare (Just key) currentKey == GT then
                        helper (Just key) (Just y) ys
                    else
                        helper currentKey currentVal ys
    in
    helper Nothing Nothing list
