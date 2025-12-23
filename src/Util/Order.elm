module Util.Order exposing (..)

minWith : (a -> a -> Order) -> a -> a -> a
minWith keyFunc a b =
    case keyFunc a b of
        LT ->
            a
        EQ ->
            a
        GT ->
            b

maxWith : (a -> a -> Order) -> a -> a -> a
maxWith keyFunc a b =
    case keyFunc a b of
        GT ->
            a
        EQ ->
            a
        LT ->
            b