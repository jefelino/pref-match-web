module Matching exposing (Assignment, Results, InputData, Course, Person, Rank, Count, AssignmentRecord, DistributionRecord, solve, State, initState, step, finish, progress)

{-| Data structures for preference-based matching

# Types
@docs Assignment, Results, InputData, Course, Person, Rank, Count, AssignmentRecord, DistributionRecord

# Functions
@docs solve

-}

import Dict exposing (Dict)
import MyList as List
import Random
import Shuffle


-- TYPES


{-| A preference ranking (lower is better, e.g., 1 is top choice)
-}
type alias Rank =
    Int


{-| A count of people with a particular ranking
-}
type alias Count =
    Int

type alias Person = String
type alias Course = String

type alias InputData =
    { courses : Dict Course Count
    , preferences : Dict (Person, Course) { rank: Rank, fixed : Bool}
    }


{-| A single assignment record for output/display -}
type alias AssignmentRecord =
    { course : Course
    , person : Person
    , rank : Maybe Rank  -- Maybe because assignment might not have a preference entry
    }

{-| A distribution entry showing how many people got each rank.
The rank field is Int (not Maybe Int) since this is for display of actual numeric ranks.
-}
type alias DistributionRecord =
    { rank : Int
    , count : Count
    }


type alias Results =
    { assignments : List AssignmentRecord
    , distribution : List DistributionRecord
    }


{-| Internal search state: maps each person to their assigned course -}
type alias Assignment = Dict Person Course


{-| Solver state tracking multiple runs to find optimal solutions -}
type State
    = State
        { inputData : InputData
        , bestAssignments : List Assignment
        , bestDist : (Dist, Count)  -- Best distribution and constraint violation count
        , runsCompleted : Int
        , goalRuns : Int
        }


-- SOLVING


{-| Preference distribution: maps each numeric ranking to the count of people with that ranking.
For example: {1: 3, 2: 2, 5: 1} means 3 people got rank 1, 2 got rank 2, 1 got rank 5.
-}
type alias Dist =
    Dict Int Count

ranks : InputData -> Assignment -> List (Maybe Rank)
ranks {preferences} assignment =
    assignment
        |> Dict.toList
        |> List.map
            (\(person, course) ->
                Dict.get (person, course) preferences
                    |> Maybe.map .rank
            )
                    

{-| Convert a list of Maybe rankings to a preference distribution.
Returns a tuple of (distribution, countOfNothings).
Nothing values represent assignments with no preference entry (forbidden).
-}
preferenceDistribution : List (Maybe Rank) -> (Dist, Count)
preferenceDistribution rankings =
    let
        (dist, nothingCount) =
            List.foldl
                (\maybeRank (accDist, accNothings) ->
                    case maybeRank of
                        Nothing ->
                            (accDist, accNothings + 1)
                        
                        Just rank ->
                            let
                                newDist =
                                    Dict.update rank
                                        (\maybeCount ->
                                            case maybeCount of
                                                Nothing ->
                                                    Just 1
                                                Just count ->
                                                    Just (count + 1)
                                        )
                                        accDist
                            in
                            (newDist, accNothings)
                )
                (Dict.empty, 0)
                rankings
    in
    (dist, nothingCount)


{-| Compare two distributions lexicographically from worst to best rank.

A distribution is better if at the worst rank where they differ, it has fewer people.

Returns:
- GT if distA is better than distB
- LT if distB is better than distA
- EQ if they are equal

For example:
- [5,5,2,1] is better than [6,3,2,1] because the worst rank is better (5 < 6)
- [4,4,2,1] is better than [4,3,3,1] because at rank 4 they're equal (both have 1 person)
  but at rank 3, the first has 0 people and the second has 2 people
-}
compareLex : Dist -> Dist -> Order
compareLex distA distB =
    let
        -- Build list of comparisons (worst-to-best order due to prepending)
        comparisons =
            Dict.merge
                -- Notice the order: if there are *more* people at a rank, this is *worse* (LT)
                -- Left only: distA has this rank, distB doesn't (countB = 0)
                (\_ countA acc -> compare 0 countA :: acc)
                -- Both: compare counts
                (\_ countA countB acc -> compare countB countA :: acc)
                -- Right only: distB has this rank, distA doesn't (countA = 0)
                (\_ countB acc -> compare countB 0 :: acc)
                distA
                distB
                []
    in
    -- Find first non-equal comparison
    comparisons
        |> List.find (\ord -> ord /= EQ)
        |> Maybe.withDefault EQ


{-| Compare two lists of rankings using leximin social choice criterion.

Comparison rules:
1. First, compare by number of constraint violations (fewer Nothings is better)
2. If equal, compare by total size (more people assigned is better)
3. If sizes are equal, compare lexicographically from worst to best rank

Returns:
- GT if rankings1 is better than rankings2
- LT if rankings2 is better than rankings1
- EQ if they are equal
-}
compareLeximin : List (Maybe Rank) -> List (Maybe Rank) -> Order
compareLeximin rankings1 rankings2 =
    let
        (dist1, nothings1) = preferenceDistribution rankings1
        (dist2, nothings2) = preferenceDistribution rankings2
    in
    case compare nothings1 nothings2 of
        LT ->
            -- Fewer constraint violations is better
            GT
        
        GT ->
            LT
        
        EQ ->
            compareLex dist1 dist2


{-| Try to improve an assignment by finding a beneficial pairwise swap.

Checks all pairs of people (i, j) to see if swapping their assignments
would improve the overall leximin score. Respects fixed constraints.

Returns Just (improved assignment) if an improvement is found, Nothing otherwise.
-}
tryToImprove : InputData -> Assignment -> Maybe Assignment
tryToImprove inputData assignment =
    let
        -- Filter out people with fixed assignments
        unfixedPeople =
            Dict.keys assignment
                |> List.filter
                    (\person ->
                        case Dict.get person assignment of
                            Just course ->
                                Dict.get (person, course) inputData.preferences
                                    |> Maybe.map .fixed
                                    |> Maybe.withDefault False
                                    |> not
                            Nothing ->
                                False
                    )
        
        -- Generate all pairs of unfixed people
        allPairs =
            List.concatMap
                (\person1 ->
                    List.filterMap
                        (\person2 ->
                            if person1 < person2 then
                                Just (person1, person2)
                            else
                                Nothing
                        )
                        unfixedPeople
                )
                unfixedPeople
    in
    -- Find the first improving swap
    List.findMap (\(p1, p2) -> trySwap inputData assignment p1 p2) allPairs


{-| Try swapping two people's assignments and check if it improves leximin -}
trySwap : InputData -> Assignment -> Person -> Person -> Maybe Assignment
trySwap inputData assignment person1 person2 =
    case (Dict.get person1 assignment, Dict.get person2 assignment) of
        (Just course1, Just course2) ->
            -- Don't swap if they have the same assignment
            if course1 == course2 then
                Nothing
            else
                -- Get current and new ranks (Nothing if no preference entry exists)
                let
                    rank1Old = Dict.get (person1, course1) inputData.preferences |> Maybe.map .rank
                    rank2Old = Dict.get (person2, course2) inputData.preferences |> Maybe.map .rank
                    rank1New = Dict.get (person1, course2) inputData.preferences |> Maybe.map .rank
                    rank2New = Dict.get (person2, course1) inputData.preferences |> Maybe.map .rank
                    
                    oldRankings = [rank1Old, rank2Old]
                    newRankings = [rank1New, rank2New]
                in
                -- Swap if it improves the leximin score
                -- (leximin automatically penalizes Nothing values)
                if compareLeximin newRankings oldRankings == GT then
                    Just (assignment
                        |> Dict.insert person1 course2
                        |> Dict.insert person2 course1)
                else
                    Nothing
        
        _ ->
            -- One or both people not in assignment
            Nothing

{-| Initialize a greedy assignment from input data.

The greedy algorithm processes preferences in rounds:
1. First, assign all fixed constraints
2. Then, for each preference rank (1, 2, 3, ...), assign people who want that rank
3. Within each round, process people in a random order to reduce order bias
4. Finally, assign any remaining unassigned people to their best available course

Returns a Random generator that produces the initial assignment.
-}
init : InputData -> Random.Generator Assignment
init inputData =
    Random.map (greedyAssignment inputData) (shufflePeople inputData)


{-| Get all unique people from the input data -}
getAllPeople : InputData -> List Person
getAllPeople inputData =
    inputData.preferences
        |> Dict.keys
        |> List.map Tuple.first
        |> List.foldl (\person set -> Dict.insert person () set) Dict.empty
        |> Dict.keys


{-| Shuffle the list of people -}
shufflePeople : InputData -> Random.Generator (List Person)
shufflePeople inputData =
    let
        people = getAllPeople inputData
    in
    Shuffle.shuffle people


{-| Perform greedy assignment with a given order of people -}
greedyAssignment : InputData -> List Person -> Assignment
greedyAssignment inputData shuffledPeople =
    let
        -- Track remaining slots for each course
        initialSlots = inputData.courses
        
        -- First, assign all fixed constraints
        (fixedAssignments, slotsAfterFixed) =
            assignFixedConstraints inputData initialSlots
        
        -- Get people who aren't already assigned by constraints
        fixedPeople = Dict.keys fixedAssignments
        unassignedPeople = 
            List.filter (\p -> not (List.member p fixedPeople)) shuffledPeople
        
        -- Process preferences round by round
        maxRank = getMaxRank inputData
        finalAssignment =
            List.range 1 maxRank
                |> List.foldl 
                    (\rank (assignment, slots) -> 
                        assignRound inputData rank unassignedPeople assignment slots
                    )
                    (fixedAssignments, slotsAfterFixed)
                |> Tuple.first
    in
    finalAssignment


{-| Get the maximum preference rank in the data -}
getMaxRank : InputData -> Int
getMaxRank inputData =
    inputData.preferences
        |> Dict.values
        |> List.map .rank
        |> List.maximum
        |> Maybe.withDefault 1


{-| Assign all fixed constraints first -}
assignFixedConstraints : InputData -> Dict Course Count -> (Assignment, Dict Course Count)
assignFixedConstraints inputData slots =
    inputData.preferences
        |> Dict.toList
        |> List.filter (\(_, pref) -> pref.fixed)
        |> List.foldl
            (\((person, course), _) (assignment, remainingSlots) ->
                let
                    currentSlots = Dict.get course remainingSlots |> Maybe.withDefault 0
                in
                if currentSlots > 0 then
                    ( Dict.insert person course assignment
                    , Dict.insert course (currentSlots - 1) remainingSlots
                    )
                else
                    -- No slots available, skip this constraint
                    (assignment, remainingSlots)
            )
            (Dict.empty, slots)


{-| Assign people who want a specific preference rank -}
assignRound : InputData -> Int -> List Person -> Assignment -> Dict Course Count -> (Assignment, Dict Course Count)
assignRound inputData targetRank unassignedPeople currentAssignment currentSlots =
    unassignedPeople
        |> List.foldl
            (\person (assignment, slots) ->
                -- Skip if already assigned
                if Dict.member person assignment then
                    (assignment, slots)
                else
                    -- Find courses this person ranked at targetRank that have slots
                    case findCourseWithRank inputData person targetRank slots of
                        Just course ->
                            let
                                remainingSlots = Dict.get course slots |> Maybe.withDefault 0
                            in
                            ( Dict.insert person course assignment
                            , Dict.insert course (remainingSlots - 1) slots
                            )
                        
                        Nothing ->
                            (assignment, slots)
            )
            (currentAssignment, currentSlots)


{-| Find a course that a person ranked at the target rank and has available slots -}
findCourseWithRank : InputData -> Person -> Int -> Dict Course Count -> Maybe Course
findCourseWithRank inputData person targetRank slots =
    inputData.preferences
        |> Dict.toList
        |> List.filterMap
            (\((p, course), pref) ->
                if p == person && pref.rank == targetRank then
                    let
                        availableSlots = Dict.get course slots |> Maybe.withDefault 0
                    in
                    if availableSlots > 0 then
                        Just course
                    else
                        Nothing
                else
                    Nothing
            )
        |> List.head


{-| Initialize the solver state with the given number of goal runs -}
initState : InputData -> Int -> State
initState inputData goalRuns =
    State
        { inputData = inputData
        , bestAssignments = []
        , bestDist = (Dict.empty, 999999)  -- Worst possible initial state
        , runsCompleted = 0
        , goalRuns = goalRuns
        }


{-| Run one iteration of the solver, generating and improving an assignment -}
step : State -> Random.Generator State
step (State state) =
    let
        assignment = findAssignment state.inputData
    in
    assignment
        |> Random.map
            (\newAssignment ->
                let
                    -- Get all ranks from this assignment                    
                    newDist = preferenceDistribution (ranks state.inputData newAssignment)
                    
                    -- Compare this result with the best so far
                    comparison = compareDist newDist state.bestDist
                in
                if comparison == GT then
                    -- Found a better solution
                    State
                        { state
                            | bestAssignments = [newAssignment]
                            , bestDist = newDist
                            , runsCompleted = state.runsCompleted + 1
                        }
                else if comparison == EQ then
                    -- Found an equally good solution
                    State
                        { state
                            | bestAssignments = newAssignment :: state.bestAssignments
                            , runsCompleted = state.runsCompleted + 1
                        }
                else
                    -- Not as good, just increment counter
                    State { state | runsCompleted = state.runsCompleted + 1 }
            )


{-| Compare two distributions (with constraint counts) -}
compareDist : (Dist, Count) -> (Dist, Count) -> Order
compareDist (distA, nothingsA) (distB, nothingsB) =
    case compare nothingsA nothingsB of
        LT ->
            GT
        
        GT ->
            LT
        
        EQ ->
            compareLex distA distB


{-| Check if we've completed enough runs and return results if so -}
finish : State -> Maybe Results
finish (State state) =
    if state.runsCompleted >= state.goalRuns then
        case state.bestAssignments of
            firstAssignment :: _ ->
                Just (assignmentToResults state.inputData firstAssignment)
            
            [] ->
                Nothing
    else
        Nothing


{-| Get the current progress as a fraction between 0.0 and 1.0 -}
progress : State -> Float
progress (State state) =
    if state.goalRuns > 0 then
        toFloat state.runsCompleted / toFloat state.goalRuns
    else
        1.0


{-| Find an optimal assignment using leximin optimization

Generates an initial greedy assignment, then iteratively improves it
by finding beneficial pairwise swaps until no improvements are possible.
-}
findAssignment : InputData -> Random.Generator Assignment
findAssignment inputData =
    init inputData
        |> Random.map (improve inputData)


{-| Iteratively improve an assignment until no beneficial swaps remain -}
improve : InputData -> Assignment -> Assignment
improve inputData assignment =
    case tryToImprove inputData assignment of
        Just betterAssignment ->
            improve inputData betterAssignment
        
        Nothing ->
            assignment


{-| Solve the matching problem using leximin optimization

Returns a Random generator that produces Results with assignments and distribution.
Runs 1000 passes with different random orderings and keeps the best result.
-}
solve : InputData -> Random.Generator Results
solve inputData =
    let
        -- Run multiple passes and keep the best
        runMultiplePasses : Int -> Assignment -> (Dist, Count) -> Random.Generator (Assignment, (Dist, Count))
        runMultiplePasses remainingRuns bestAssignment bestDist =
            if remainingRuns <= 0 then
                Random.constant (bestAssignment, bestDist)
            else
                findAssignment inputData
                    |> Random.andThen
                        (\newAssignment ->
                            let
                                newDist = preferenceDistribution (ranks inputData newAssignment)
                                
                                -- Compare new result with best
                                comparison = compareDist newDist bestDist
                            in
                            case comparison of
                                GT ->
                                    -- New assignment is better
                                    runMultiplePasses (remainingRuns - 1) newAssignment newDist
                                
                                _ ->
                                    -- Keep the best
                                    runMultiplePasses (remainingRuns - 1) bestAssignment bestDist
                        )
    in
    -- Start with first assignment
    findAssignment inputData
        |> Random.andThen
            (\initialAssignment ->
                let
                    initialDist = preferenceDistribution (ranks inputData initialAssignment)    
                in
                runMultiplePasses 999 initialAssignment initialDist
                    |> Random.map (\(finalAssignment, _) -> assignmentToResults inputData finalAssignment)
            )




{-| Convert an Assignment to Results format with assignments and distribution -}
assignmentToResults : InputData -> Assignment -> Results
assignmentToResults inputData assignment =
    let
        -- Build list of assignment records
        assignmentRecords =
            assignment
                |> Dict.toList
                |> List.map
                    (\(person, course) ->
                        { person = person
                        , course = course
                        , rank = Dict.get (person, course) inputData.preferences |> Maybe.map .rank
                        }
                    )
                |> List.sortBy (\r -> (r.course, r.person))
        
        -- Build distribution: count how many people got each rank
        (dist, _) = preferenceDistribution (ranks inputData assignment)
        
        distributionRecords =
            dist
                |> Dict.toList
                |> List.map (\(rank, count) -> { rank = rank, count = count })
                |> List.sortBy .rank
    in
    { assignments = assignmentRecords
    , distribution = distributionRecords
    }
