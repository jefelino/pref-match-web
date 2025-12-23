module Validate exposing (validate, ValidationResult)

{-| Validation for input data

@docs validate, ValidationResult

-}

import Dict
import Matching exposing (InputData)


{-| Result of validation: cleaned data and any warnings -}
type alias ValidationResult =
    { data : InputData
    , warnings : List String
    }


{-| Validate and clean input data, returning warnings for issues that don't prevent execution

Validations performed:
- Check if number of people matches total slots (warn if mismatch)
- Check for people with multiple fixed constraints (warn, keep only first)
- Check for invalid rankings (warn, fix by normalizing)
- Check for gaming attempts (warn if not enough courses ranked between 1 and n)
-}
validate : InputData -> ValidationResult
validate inputData =
    let
        -- Start with the input data and empty warnings
        result = { data = inputData, warnings = [] }
    in
    result
        |> checkPeopleVsSlots
        |> checkMultipleFixedConstraints
        |> checkRankingValidity


{-| Check if number of people matches total slots -}
checkPeopleVsSlots : ValidationResult -> ValidationResult
checkPeopleVsSlots result =
    let
        totalSlots =
            Dict.values result.data.courses |> List.sum
        
        numPeople =
            result.data.preferences
                |> Dict.keys
                |> List.map Tuple.first
                |> List.foldl (\person set -> Dict.insert person () set) Dict.empty
                |> Dict.size
    in
    if numPeople /= totalSlots then
        let
            warning =
                if numPeople < totalSlots then
                    String.fromInt (totalSlots - numPeople) ++ " course slots will be unfilled (" 
                        ++ String.fromInt numPeople ++ " people, " 
                        ++ String.fromInt totalSlots ++ " total slots)"
                else
                    String.fromInt (numPeople - totalSlots) ++ " people will be unassigned (" 
                        ++ String.fromInt numPeople ++ " people, " 
                        ++ String.fromInt totalSlots ++ " total slots)"
        in
        { result | warnings = result.warnings ++ [warning] }
    else
        result


{-| Check for people with multiple fixed constraints and keep only the first one -}
checkMultipleFixedConstraints : ValidationResult -> ValidationResult
checkMultipleFixedConstraints result =
    let
        -- Group fixed assignments by person
        fixedByPerson =
            result.data.preferences
                |> Dict.toList
                |> List.filter (\(_, pref) -> pref.fixed)
                |> List.foldl
                    (\((person, course), pref) acc ->
                        Dict.update person
                            (\maybeList ->
                                case maybeList of
                                    Nothing -> Just [(course, pref)]
                                    Just list -> Just (list ++ [(course, pref)])
                            )
                            acc
                    )
                    Dict.empty
        
        -- Find people with multiple fixed constraints
        peopleWithMultiple =
            fixedByPerson
                |> Dict.filter (\_ courses -> List.length courses > 1)
                |> Dict.toList
        
        -- Generate warnings
        warnings =
            peopleWithMultiple
                |> List.map
                    (\(person, courses) ->
                        let
                            courseNames = List.map Tuple.first courses
                            firstCourse = List.head courseNames |> Maybe.withDefault "unknown"
                        in
                        person ++ " has multiple fixed constraints (" 
                            ++ String.join ", " courseNames 
                            ++ "). Using only the first: " ++ firstCourse
                    )
        
        -- Clean up data: for each person with multiple fixed, unfix all but the first
        cleanedPreferences =
            if List.isEmpty peopleWithMultiple then
                result.data.preferences
            else
                result.data.preferences
                    |> Dict.map
                        (\(person, course) pref ->
                            if pref.fixed then
                                case Dict.get person fixedByPerson of
                                    Just fixedCourses ->
                                        let
                                            firstFixedCourse = 
                                                List.head fixedCourses 
                                                    |> Maybe.map Tuple.first 
                                                    |> Maybe.withDefault ""
                                        in
                                        -- Keep fixed only if this is the first fixed course
                                        { pref | fixed = course == firstFixedCourse }
                                    
                                    Nothing ->
                                        pref
                            else
                                pref
                        )
    in
    { result 
        | warnings = result.warnings ++ warnings
        , data = { courses = result.data.courses, preferences = cleanedPreferences }
    }


{-| Check ranking validity and normalize problematic rankings

Checks:
1. Rankings should be in a reasonable range (1 to number of courses)
2. Rankings should satisfy: for each n, at least n courses ranked ≤n (prevents gaming)
   This allows ties (e.g., [1,1,3,4] is valid - two 1sts, then 3rd, then 4th)

Fixes:
- Out-of-range ranks are moved to last place
- Rankings that violate the ≥n ranked ≤n rule are renormalized
-}
checkRankingValidity : ValidationResult -> ValidationResult
checkRankingValidity result =
    let
        numCourses = Dict.size result.data.courses
        
        -- Get all people
        allPeople =
            result.data.preferences
                |> Dict.keys
                |> List.map Tuple.first
                |> List.foldl (\person set -> Dict.insert person () set) Dict.empty
                |> Dict.keys
        
        -- Check each person's rankings
        (cleanedPreferences, newWarnings) =
            allPeople
                |> List.foldl
                    (\person (prefs, warnings) ->
                        let
                            personPrefs =
                                prefs
                                    |> Dict.toList
                                    |> List.filter (\((p, _), _) -> p == person)
                            
                            -- Extract ranks for validation
                            ranks = List.map (\(_, pref) -> pref.rank) personPrefs
                            
                            -- Check for out-of-range ranks
                            outOfRange = List.filter (\r -> r < 1 || r > numCourses) ranks
                            
                            -- Check the "≥n ranked ≤n" rule: for each n from 1 to numCourses,
                            -- there should be at least n courses ranked in [1, n]
                            -- This allows ties (e.g., two 1st places means next is 3rd)
                            firstGap =
                                List.range 1 numCourses
                                    |> List.filter
                                        (\n ->
                                            let
                                                countInRange = List.filter (\r -> r >= 1 && r <= n) ranks |> List.length
                                            in
                                            countInRange < n
                                        )
                                    |> List.head
                            
                            -- Generate warnings
                            personWarnings =
                                if not (List.isEmpty outOfRange) then
                                    [ person ++ " has out-of-range rankings (" 
                                        ++ String.join ", " (List.map String.fromInt outOfRange) 
                                        ++ "). These will be treated as last place." ]
                                else if firstGap /= Nothing then
                                    let
                                        gapN = Maybe.withDefault 0 firstGap
                                    in
                                    [ person ++ " included fewer than " 
                                        ++ String.fromInt gapN 
                                        ++ " courses at rank " ++ String.fromInt gapN 
                                        ++ " or below. Ties have been renumbered." ]
                                else
                                    []
                            
                            -- Fix out-of-range rankings or renormalize if needed
                            fixedPrefs =
                                if not (List.isEmpty outOfRange) then
                                    prefs
                                        |> Dict.map
                                            (\(p, _) pref ->
                                                if p == person then
                                                    let
                                                        r = pref.rank
                                                    in
                                                    if r < 1 || r > numCourses then
                                                        { pref | rank = numCourses + 1 }
                                                    else
                                                        pref
                                                else
                                                    pref
                                            )
                                else if firstGap /= Nothing then
                                    -- Renormalize: for each course, new rank = (number of courses ranked strictly better) + 1
                                    -- Example: [1, 6, 6, 6, 3, 1, 4, 5] → [1, 6, 6, 6, 3, 1, 4, 5] (already valid)
                                    -- Example: [1, 2, 3, 4, 5, 8, 8, 8] → [1, 2, 3, 4, 5, 6, 6, 6]
                                    prefs
                                        |> Dict.map
                                            (\(p, _) pref ->
                                                if p == person then
                                                    let
                                                        r = pref.rank
                                                        -- Count how many courses this person ranked strictly better
                                                        numBetter = List.filter (\rank -> rank < r) ranks |> List.length
                                                    in
                                                    { pref | rank = numBetter + 1 }
                                                else
                                                    pref
                                            )
                                else
                                    prefs
                        in
                        (fixedPrefs, warnings ++ personWarnings)
                    )
                    (result.data.preferences, [])
    in
    { result 
        | warnings = result.warnings ++ newWarnings
        , data = { courses = result.data.courses, preferences = cleanedPreferences }
    }

