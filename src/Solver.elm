module Solver exposing (SearchState, initState, stepState, result, finished)

{-| Deterministic leximin solver using branch-and-bound

This module will implement an exact algorithm:
1. Build assignments incrementally (one person at a time)
2. Prune branches that provably can't beat current best
3. Guaranteed to find the true leximin optimum

@docs solve
-}

import Dict exposing (Dict)
import Util.MyDict as Dict
import Util.MyMaybe as Maybe
import Util.MyList as List
import Util.Order as Order
import Dist
import Matching exposing (InputData, Result, Dist, Person, Course, Rank, Count, noResults, compareLex, assignmentToDist,compareLex, joinResults)

type alias Prefs = Dict (Person, Course) Rank

type alias Slots = Dict Course Count

type alias Assignment = Dict Person (Course, Rank)

type alias SearchSpace = 
    { preferences : Prefs
    , slots : Slots
    , assignment : Assignment
    }

type SearchPosition 
    = Step SearchSpace SearchPosition
    | Finish

type SearchState = SearchState
    { position : SearchPosition
    , resultsSoFar : Result
    }

initState : InputData -> SearchState
initState inputData = 
    SearchState
        { position = Step (initSpace inputData) Finish
        , resultsSoFar = noResults
        }

result : SearchState -> Result
result (SearchState { resultsSoFar }) =
    resultsSoFar

finished : SearchState -> Bool
finished (SearchState { position }) =
    case position of
        Finish ->
            True
        _ ->
            False

assignmentToResult : Assignment -> Result
assignmentToResult assignment =
    Just (assignmentToDist assignment, [assignment])

stepState : SearchState -> Maybe SearchState
stepState (SearchState state) =
    case state.position of
        Finish ->
            Nothing

        Step space next ->
            case select space of
                Nothing ->
                    -- Check if all slots are filled (0 slots remaining for all courses)
                    let
                        allSlotsFilled =
                            space.slots
                                |> Dict.values
                                |> List.all ((==) 0)
                    in
                    if allSlotsFilled then
                        -- Valid complete assignment
                        Just (SearchState { state 
                            | resultsSoFar = joinResults (assignmentToResult space.assignment) state.resultsSoFar
                            , position = next 
                            })
                    else
                        -- Infeasible branch - skip to next
                        Just (SearchState { state | position = next })
                
                Just ((person, course, rank), dist) ->
                    if prune state.resultsSoFar dist then
                        -- Prune this branch
                        Just (SearchState { state | position = next })
                    else
                        let 
                            newSpace = assign person course rank space

                            nextPref = Dict.remove (person, course) space.preferences

                            nextStep = Step { space | preferences = nextPref } next
                        in
                        Just (SearchState { state | position = Step newSpace nextStep })


{-| Select the next (person, course, rank) to try assigning,
    and the best case distribution achievable from there.
    We find the course with the worst remaining rank, and assign it to the
    person who ranked it highest.
-}
select : SearchSpace -> Maybe ((Person, Course, Rank), Maybe Dist)
select space =
    let
        bestForEachCourse : Dict Course (List (Person, Rank))
        bestForEachCourse =
            Dict.foldl
                (\(person, course) rank acc ->
                    Dict.update course (
                        Maybe.withDefault [] 
                        >> (::) (person, rank)
                        >> List.sortBy Tuple.second
                        >> List.take (Dist.get course space.slots)
                        >> Just
                    ) acc
                )
                Dict.empty
                space.preferences
        
        filledEverySlot : Bool
        filledEverySlot =
            space.slots
                |> Dict.toList
                |> List.all (\(course, slotsNeeded) -> 
                    let
                        candidates = Dict.get course bestForEachCourse
                            |> Maybe.withDefault []
                    in
                    List.length candidates == slotsNeeded
                    ) 
                
        dist : Maybe Dist
        dist = 
            if filledEverySlot then
                bestForEachCourse
                    |> Dict.values
                    |> List.concat
                    |> List.map Tuple.second
                    |> Dist.count
                    |> Order.maxWith compareLex personwiseDist
                    |> Dist.join (assignmentToDist space.assignment)
                    |> Just
            else
                Nothing
            
        -- Check a different way: assign each person their best remaining course
        personwiseDist : Dist
        personwiseDist =
            space.preferences
                |> Dict.foldl (\(person, _) rank acc ->
                    Dict.update person (Maybe.min (Just rank)) acc
                ) Dist.empty
                |> Dict.values
                |> Dist.count
    

        worstCourseWithBestPerson =
            bestForEachCourse
                |> Dict.toList
                |> List.filterMap (\(course, options) -> 
                    List.head options 
                    |> Maybe.map (\(person, rank) -> (person, course, rank))
                    )
                |> List.maximumBy (\(_, _, rank) -> rank)
    in
    worstCourseWithBestPerson
        |> Maybe.map (\selection -> (selection, dist)) 

prune : Result -> Maybe Dist -> Bool
prune resultSoFar maybeBestPossible =
    case (resultSoFar, maybeBestPossible) of
        (_, Nothing) ->
            True
        
        (Nothing, _) ->
            False
        
        (Just (bestSoFar, _), Just best) ->
            -- Prune if the best possible in this branch is worse than what we've already found
            compareLex best bestSoFar == GT

initSpace : InputData -> SearchSpace
initSpace {preferences, courses} =
    let
        space =
            { preferences = Dict.map (\_ { rank } -> rank) preferences
            , slots = courses
            , assignment = Dict.empty
            }
    in
        Dict.foldl fixPref space preferences

assign : Person -> Course -> Rank -> SearchSpace -> SearchSpace
assign person course rank space =
    let
        newSlots =
            Dist.decrement course space.slots

        newPrefs =
            space.preferences
                |> Dict.filter (\(p, c) _ -> p /= person && Dist.get c newSlots > 0)
        
        newAssignment =
            Dict.insert person (course, rank) space.assignment
    in
    { preferences = newPrefs
    , slots = newSlots
    , assignment = newAssignment
    } 

fixPref : (Person, Course) -> { rank : Rank, fixed : Bool } -> SearchSpace -> SearchSpace
fixPref (person, course) { rank, fixed } space =
    if fixed then
        assign person course rank space
    else
        space

