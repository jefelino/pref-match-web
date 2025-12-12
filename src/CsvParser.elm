module CsvParser exposing (parse)

{-| CSV parsing for preference data

@docs parse

-}

import Dict exposing (Dict)
import Matching exposing (Course, Person, InputData, Rank, Count)


{-| Parse CSV content into InputData

Expected format:
- Row 1: "Courses", followed by course names
- Row 2: "Number of slots", followed by slot counts
- Remaining rows: Person name, followed by preference ranks (1, 2, 3...) or fixed assignments (*1, *2, etc.)

-}
parse : String -> Result String InputData
parse csvContent =
    let
        lines =
            String.lines csvContent
                |> List.map String.trim
                |> List.filter (not << String.isEmpty)
    in
    case lines of
        courseRow :: slotRow :: peopleRows ->
            parseCsvRows courseRow slotRow peopleRows

        _ ->
            Err "CSV must have at least 2 rows (courses and slots)"


parseCsvRows : String -> String -> List String -> Result String InputData
parseCsvRows courseRow slotRow peopleRows =
    let
        courseNames =
            splitCsv courseRow
                |> List.drop 1
                |> List.map String.trim
        
        coursesResult =
            parseCourses courseRow slotRow
        
        preferencesResult =
            parsePeoplePreferences courseNames peopleRows
    in
    case (coursesResult, preferencesResult) of
        (Ok courses, Ok preferences) ->
            Ok { courses = courses, preferences = preferences }
        
        (Err err, _) ->
            Err err
        
        (_, Err err) ->
            Err err


parseCourses : String -> String -> Result String (Dict Course Count)
parseCourses courseRow slotRow =
    let
        courseNames =
            splitCsv courseRow
                |> List.drop 1  -- Skip "Courses" label
                |> List.map String.trim
        
        slotCounts =
            splitCsv slotRow
                |> List.drop 1  -- Skip "Number of slots" label
                |> List.map (String.trim >> String.toInt)
    in
    if List.length courseNames /= List.length slotCounts then
        Err "Number of courses doesn't match number of slot values"
    else
        let
            combined =
                List.map2 Tuple.pair courseNames slotCounts
            
            invalidSlots =
                List.filter (\(_, slot) -> slot == Nothing) combined
        in
        if not (List.isEmpty invalidSlots) then
            Err "Invalid slot count (must be a number)"
        else
            Ok (List.filterMap toCourseEntry combined |> Dict.fromList)


toCourseEntry : (String, Maybe Int) -> Maybe (Course, Count)
toCourseEntry (name, maybeSlots) =
    Maybe.map (\slots -> (name, slots)) maybeSlots


parsePeoplePreferences : List Course -> List String -> Result String (Dict (Person, Course) { rank : Rank, fixed : Bool })
parsePeoplePreferences courseNames peopleRows =
    peopleRows
        |> List.map (parsePersonPreferences courseNames)
        |> combineResults
        |> Result.map (List.concat >> Dict.fromList)


parsePersonPreferences : List Course -> String -> Result String (List ((Person, Course), { rank : Rank, fixed : Bool }))
parsePersonPreferences courseNames row =
    let
        values =
            splitCsv row
    in
    case values of
        [] ->
            Err "Empty row"
        
        name :: preferenceStrings ->
            if List.length courseNames /= List.length preferenceStrings then
                Err "Number of preferences doesn't match number of courses"
            else
                let
                    personName =
                        String.trim name
                in
                List.map2 (parsePreference personName) courseNames preferenceStrings
                    |> combineResults
                    |> Result.map (List.filterMap identity)


parsePreference : Person -> Course -> String -> Result String (Maybe ((Person, Course), { rank : Rank, fixed : Bool }))
parsePreference personName courseName prefString =
    let
        trimmed =
            String.trim prefString
        
        isFixed =
            String.startsWith "*" trimmed
        
        isNegative =
            String.startsWith "-" trimmed
        
        rankString =
            if isFixed then
                String.dropLeft 1 trimmed
            else if isNegative then
                String.dropLeft 1 trimmed  -- Drop the '-', ignore any following number
            else
                trimmed
    in
    if isNegative then
        -- Negative constraint: don't create an entry (represented by Nothing)
        Ok Nothing
    else
        case String.toInt rankString of
            Just rank ->
                Ok (Just ((personName, courseName), { rank = rank, fixed = isFixed }))
            
            Nothing ->
                Err ("Invalid preference rank: " ++ prefString)


splitCsv : String -> List String
splitCsv line =
    String.split "," line


combineResults : List (Result e a) -> Result e (List a)
combineResults results =
    List.foldr
        (\result acc ->
            case (result, acc) of
                (Ok value, Ok values) ->
                    Ok (value :: values)
                
                (Err err, _) ->
                    Err err
                
                (_, Err err) ->
                    Err err
        )
        (Ok [])
        results
