module Matching exposing 
    ( InputData, Result, Dist, Course, Person, Rank, Count, tidy, noResults, joinResults, assignmentToDist, compareLex, resultDist
    )

import Dict exposing (Dict)
import Dist exposing (Dist)


{-| A preference ranking (lower is better, e.g., 1 is top choice)
-}
type alias Rank =
    Int

{-| A count of people with a particular ranking
-}
type alias Count =
    Int

type alias Dist = Dist.Dist Rank

type alias Person = String
type alias Course = String

type alias InputData =
    { courses : Dict Course Count
    , preferences : Dict (Person, Course) { rank: Rank, fixed : Bool}
    }

type alias Assignment = Dict Person (Course, Rank)

type alias Result =
    Maybe (Dist, List Assignment)

noResults : Result
noResults =
    Nothing


{-| Compare two distributions lexicographically from worst to best rank.
GT means that distA is *worse* than distB, since it has more people at its worst rank. -}
compareLex : Dist -> Dist -> Order
compareLex distA distB =
    let
        comparisons =
            Dict.merge
                (\_ countA acc -> compare countA 0 :: acc)
                (\_ countA countB acc -> compare countA countB :: acc)
                (\_ countB acc -> compare 0 countB :: acc)
                distA
                distB
                []
    in
    comparisons
        |> List.filter (\ord -> ord /= EQ)
        |> List.head
        |> Maybe.withDefault EQ



joinResults : Result -> Result -> Result
joinResults r1 r2 =
    case (r1, r2) of
        (Nothing, Nothing) ->
            Nothing
        
        (r1val, Nothing) ->
            r1val
        
        (Nothing, r2val) ->
            r2val
        
        (Just (dist1, assignments1), Just (dist2, assignments2)) ->
            case compareLex dist1 dist2 of
                GT ->
                    Just (dist2, assignments2)
                
                LT ->
                    Just (dist1, assignments1)
                
                EQ ->
                    Just (dist1, assignments1 ++ assignments2)

resultDist : Result -> Maybe Dist
resultDist result =
    Maybe.map Tuple.first result

assignmentToDist : Assignment -> Dist
assignmentToDist assignment =
    assignment
        |> Dict.values
        |> List.map Tuple.second
        |> Dist.count

tidy : Result -> Result
tidy result =
    case result of
        Nothing ->
            Nothing
        
        Just (_, []) ->
            Nothing -- This shouldn't happen
        
        Just (dist, first :: rest) ->
            Just (dist, first :: List.map (diff first) rest)

{- Filter out all the assignments that are the same as the first. -}
diff : Assignment -> Assignment -> Assignment
diff a1 a2 =
    Dict.filter
        (\person (course1, _) ->
            case Dict.get person a1 of
                Nothing ->
                    True
                
                Just (course2, _) ->
                    course1 /= course2
        )
        a2
