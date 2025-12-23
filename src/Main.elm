port module Main exposing (main)

import Browser
import Browser.Events
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import File exposing (File)
import File.Select as Select
import Task
import Process
import Time
import Svg exposing (svg)
import Svg.Attributes as SvgAttr
import Dict exposing (Dict)
import Matching
import Solver exposing (SearchState)
import CsvParser
import Validate


-- PORTS

port copyToClipboard : String -> Cmd msg


-- MAIN

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


-- MODEL

type Model
    = NoResults
    | ShowingError String
    | ShowingResults 
        { warnings : List String
        , copyState : Maybe CopyType
        , searchState : SearchState
        }


type CopyType
    = AssignmentsCopied
    | SummaryCopied


init : () -> ( Model, Cmd Msg )
init _ =
    ( NoResults, Cmd.none )


-- UPDATE
type Msg
    = UploadRequested
    | FileSelected File
    | FileLoaded String
    | Frame Time.Posix
    | CopyToClipboard Int
    | CopySummaryToClipboard
    | CopiedMessageHide


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UploadRequested ->
            ( model, Select.file [ "text/csv" ] FileSelected )
        
        FileSelected file ->
            ( model, Task.perform FileLoaded (File.toString file) )
        FileLoaded csvContent ->
            case CsvParser.parse csvContent of
                Ok inputData ->
                    let
                        validated = Validate.validate inputData
                        search = Solver.initState validated.data
                    in
                    ( ShowingResults 
                        { warnings = validated.warnings
                        , copyState = Nothing
                        , searchState = search
                        }
                    , Cmd.none
                    )
                
                Err errorMsg ->
                    ( ShowingError errorMsg, Cmd.none )

        Frame _ ->
            case model of
                ShowingResults state ->
                    if Solver.finished state.searchState then
                        ( model, Cmd.none )
                    else
                        let
                            newSearchState = advanceMany stepsPerFrame state.searchState
                        in
                        ( ShowingResults { state | searchState = newSearchState }, Cmd.none )

                _ ->
                    ( model, Cmd.none )
        
        
        CopyToClipboard solutionIndex ->
            case model of
                ShowingResults state ->
                    case Solver.result state.searchState of
                        Just (_, assignments) ->
                            case List.head (List.drop solutionIndex assignments) of
                                Just assignment ->
                                    ( ShowingResults 
                                        { state | copyState = Just AssignmentsCopied }
                                    , Cmd.batch
                                        [ copyToClipboard (formatAssignmentForCopy assignment)
                                        , Process.sleep 2000 |> Task.perform (\_ -> CopiedMessageHide)
                                        ]
                                    )
                                
                                Nothing ->
                                    ( model, Cmd.none )
                        
                        Nothing ->
                            ( model, Cmd.none )
                
                _ ->
                    ( model, Cmd.none )
        
        CopySummaryToClipboard ->
            case model of
                ShowingResults state ->
                    case Solver.result state.searchState of
                        Just (dist, _) ->
                            ( ShowingResults 
                                { state | copyState = Just SummaryCopied }
                            , Cmd.batch
                                [ copyToClipboard (formatDistributionForCopy dist)
                                , Process.sleep 2000 |> Task.perform (\_ -> CopiedMessageHide)
                                ]
                            )
                        
                        Nothing ->
                            ( model, Cmd.none )
                
                _ ->
                    ( model, Cmd.none )
        
        CopiedMessageHide ->
            case model of
                ShowingResults state ->
                    ( ShowingResults { state | copyState = Nothing }, Cmd.none )
                
                NoResults ->
                    ( model, Cmd.none )
                
                ShowingError _ ->
                    ( model, Cmd.none )


type alias Assignment = Dict String (String, Int)

formatAssignmentForCopy : Assignment -> String
formatAssignmentForCopy assignment =
    let
        header = "Person,Course,Preference Rank"
        rows = 
            assignment
                |> Dict.toList
                |> List.map (\(person, (course, rank)) ->
                    person ++ "," ++ course ++ "," ++ String.fromInt rank
                )
    in
    String.join "\n" (header :: rows)


type alias Dist = Dict Int Int

formatDistributionForCopy : Dist -> String
formatDistributionForCopy dist =
    let
        formatDistRow (rank, count) = 
            ordinal rank ++ " choice: " ++ String.fromInt count ++ " " ++ pluralize count "person" "people"
    in
    dist
        |> Dict.toList
        |> List.sortBy Tuple.first
        |> List.map formatDistRow
        |> String.join "\n"


ordinal : Int -> String
ordinal n =
    let
        suffix =
            if modBy 100 n >= 11 && modBy 100 n <= 13 then
                "th"
            else
                case modBy 10 n of
                    1 -> "st"
                    2 -> "nd"
                    3 -> "rd"
                    _ -> "th"
    in
    String.fromInt n ++ suffix


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        ShowingResults state ->
            if Solver.finished state.searchState then
                Sub.none
            else
                Browser.Events.onAnimationFrame Frame

        _ ->
            Sub.none


-- Stepping: run multiple `stepState` calls per animation frame
stepsPerFrame : Int
stepsPerFrame = 1000

advanceMany : Int -> SearchState -> SearchState
advanceMany n initialState =
    if n <= 0 then
        initialState
    else
        case Solver.stepState initialState of
            Just newState ->
                advanceMany (n - 1) newState

            Nothing ->
                initialState


-- VIEW

view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ viewHeader
        , viewUploadSection
        , viewResults model
        , viewInstructions
        , viewStyles
        ]


viewHeader : Html Msg
viewHeader =
    div [ class "header" ]
        [ h1 [] [ text "pref-match" ]
        , p [ class "subtitle" ] 
            [ text "Assign people to positions using "
            , a [ href "https://en.wikipedia.org/wiki/Leximin_order", target "_blank" ] [ text "leximin optimization" ]
            , text ". Made by "
            , a [ href "https://jefelino.github.io", target "_blank" ] [ text "Jeff Russell" ]
            , text " and "
            , a [ href "https://claude.ai", target "_blank"] [ text "Claude" ]
            ]
        ]


viewInstructions : Html Msg
viewInstructions =
    div [ class "instructions" ]
        [ h2 [] [ text "How to use this" ]
        , p [] [ text "Upload a CSV file with each person's ranked preferences."]
        , h3 [] [ text "Example CSV" ]
        , pre [ class "example-code" ]
            [ code []
                [ text "Courses,         Course 1, Course 2, Course 3\n"
                , text "Number of slots, 2,        1,        1\n"
                , text "Person A,        1,        2,        3\n"
                , text "Person B,        2,        1,        3\n"
                , text "Person C,        3,        2,        1\n"
                , text "Person D,        1,        3,        2"
                ]
            ]
        , ul []
            [ li [] [ text "The first row lists positions." ]
            , li [] [ text "The second row shows how many people can be assigned to each position." ]
            , li [] [ text "Each following row lists a person with their rankings (1 = top choice)." ]
            ]
        , h3 [] [ text "Constraints" ]
        , ul []
            [ li [] 
                [ text "Use "
                , code [ class "inline-code" ] [ text "*" ]
                , text " to assign someone to a specific position"
                ]
            , li [] 
                [ text "Use "
                , code [ class "inline-code" ] [ text "-" ]
                , text " to prevent assignment to a position"
                ]
            ]
        , pre [ class "example-code" ]
            [ code []
                [ text "Courses,         Course 1, Course 2, Course 3\n"
                , text "Number of slots, 2,        1,        1\n"
                , text "Person A,        *1,       2,        3\n"
                , text "Person B,        2,        1,        3\n"
                , text "Person C,        2,        -,        1\n"
                , text "Person D,        1,        3,        2"
                ]
            ]
        , ul []
            [ li [] [ text "Person A must be assigned to Course 1" ]
            , li [] [ text "Person C cannot be assigned to Course 2" ]
            ]
        ]


viewUploadSection : Html Msg
viewUploadSection =
    div [ class "upload-section" ]
        [ button [ class "upload-button", onClick UploadRequested ]
            [ text "📁 Upload CSV File" ]
        ]


viewResults : Model -> Html Msg
viewResults model =
    case model of
        NoResults ->
            text ""
        
        ShowingError errorMsg ->
            div [ class "error" ]
                [ h3 [] [ text "Error parsing CSV" ]
                , p [] [ text errorMsg ]
                ]
        
        ShowingResults state ->
            case Matching.tidy (Solver.result state.searchState) of
                Nothing ->
                    if Solver.finished state.searchState then
                        div [ class "error" ]
                            [ h3 [] [ text "No solution found" ]
                            , p [] [ text "Unable to create a valid assignment with the given constraints. Try removing some constraints." ]
                            ]
                    else
                        viewProgress state.searchState
                
                Just (dist, assignments) ->
                    div [ class "results" ]
                        [ if Solver.finished state.searchState then
                            text ""
                          else
                            viewProgress state.searchState
                        , viewWarnings state.warnings
                        , if List.length assignments > 1 then
                            div [ class "solution-count-header" ]
                                [ text ("Found " ++ String.fromInt (List.length assignments) ++ " solutions tied for first place") ]
                          else
                            text ""
                        , viewDistribution dist state.copyState
                        , div [] (List.indexedMap (viewSingleAssignment state.copyState) assignments)
                        ]


viewSingleAssignment : Maybe CopyType -> Int -> Assignment -> Html Msg
viewSingleAssignment copyState index assignment =
    let
        showAssignmentCopied = 
            case copyState of
                Just AssignmentsCopied -> True
                _ -> False
        
        solutionHeader =
            if index > 0 then
                h2 [ class "solution-header" ] [ text ("Variation " ++ String.fromInt index) ]
            else
                text ""
    in
    div [ class "single-solution" ]
        [ solutionHeader
        , div [ class "results-content" ]
            [ div [ class "distribution-header" ]
                [ h3 [ class "distribution-caption" ] [ text "Assignments" ]
                , div [ class "copy-container" ]
                    [ if showAssignmentCopied then
                        span [ class "copied-message" ] [ text "Copied!" ]
                      else
                        text ""
                    , button [ class "copy-button", onClick (CopyToClipboard index), title "Copy to clipboard" ]
                        [ svg [ SvgAttr.width "16", SvgAttr.height "16", SvgAttr.viewBox "0 0 16 16", SvgAttr.fill "currentColor" ]
                            [ Svg.path [ SvgAttr.d "M0 6.75C0 5.784.784 5 1.75 5h1.5a.75.75 0 0 1 0 1.5h-1.5a.25.25 0 0 0-.25.25v7.5c0 .138.112.25.25.25h7.5a.25.25 0 0 0 .25-.25v-1.5a.75.75 0 0 1 1.5 0v1.5A1.75 1.75 0 0 1 9.25 16h-7.5A1.75 1.75 0 0 1 0 14.25Z" ] []
                            , Svg.path [ SvgAttr.d "M5 1.75C5 .784 5.784 0 6.75 0h7.5C15.216 0 16 .784 16 1.75v7.5A1.75 1.75 0 0 1 14.25 11h-7.5A1.75 1.75 0 0 1 5 9.25Zm1.75-.25a.25.25 0 0 0-.25.25v7.5c0 .138.112.25.25.25h7.5a.25.25 0 0 0 .25-.25v-7.5a.25.25 0 0 0-.25-.25Z" ] []
                            ]
                        ]
                    ]
                ]
            , viewAssignmentsTable assignment
            ]
        ]


viewWarnings : List String -> Html Msg
viewWarnings warnings =
    if List.isEmpty warnings then
        text ""
    else
        div [ class "warnings" ]
            [ h3 [] [ text "⚠️ Warnings" ]
            , ul [] (List.map (\warning -> li [] [ text warning ]) warnings)
            ]


viewDistribution : Dist -> Maybe CopyType -> Html Msg
viewDistribution dist copyState =
    let
        showCopied = 
            case copyState of
                Just SummaryCopied -> True
                _ -> False
    in
    div [ class "distribution" ]
        [ div [ class "distribution-header" ]
            [ h3 [ class "distribution-caption" ] [ text "Preference Distribution" ]
            , div [ class "copy-container" ]
                [ if showCopied then
                    span [ class "copied-message" ] [ text "Copied!" ]
                  else
                    text ""
                , button [ class "copy-button", onClick CopySummaryToClipboard, title "Copy to clipboard" ]
                    [ svg [ SvgAttr.width "16", SvgAttr.height "16", SvgAttr.viewBox "0 0 16 16", SvgAttr.fill "currentColor" ]
                        [ Svg.path [ SvgAttr.d "M0 6.75C0 5.784.784 5 1.75 5h1.5a.75.75 0 0 1 0 1.5h-1.5a.25.25 0 0 0-.25.25v7.5c0 .138.112.25.25.25h7.5a.25.25 0 0 0 .25-.25v-1.5a.75.75 0 0 1 1.5 0v1.5A1.75 1.75 0 0 1 9.25 16h-7.5A1.75 1.75 0 0 1 0 14.25Z" ] []
                        , Svg.path [ SvgAttr.d "M5 1.75C5 .784 5.784 0 6.75 0h7.5C15.216 0 16 .784 16 1.75v7.5A1.75 1.75 0 0 1 14.25 11h-7.5A1.75 1.75 0 0 1 5 9.25Zm1.75-.25a.25.25 0 0 0-.25.25v7.5c0 .138.112.25.25.25h7.5a.25.25 0 0 0 .25-.25v-7.5a.25.25 0 0 0-.25-.25Z" ] []
                        ]
                    ]
                ]
            ]
        , table [ class "results-table" ]
            [ thead []
                [ tr []
                    [ th [] [ text "Choice" ]
                    , th [] [ text "Number of people" ]
                    ]
                ]
            , tbody []
                (dist
                    |> Dict.toList
                    |> List.sortBy Tuple.first
                    |> List.map viewDistributionItem
                )
            ]
        ]


viewProgress : SearchState -> Html Msg
viewProgress _ =
    div [ class "progress-container" ]
        [ div [ class "progress-message" ] [ text "Searching..." ]
        , div [ class "progress-bar-container" ]
            [ div [ class "progress-bar indeterminate" ] [] ]
        ]


viewDistributionItem : (Int, Int) -> Html Msg
viewDistributionItem (rank, count) =
    tr []
        [ td [] [ text (ordinal rank) ]
        , td [] [ text (String.fromInt count) ]
        ]


pluralize : Int -> String -> String -> String
pluralize n singular plural =
    if n == 1 then singular else plural


viewAssignmentsTable : Assignment -> Html Msg
viewAssignmentsTable assignment =
    table [ class "results-table" ]
        [ thead []
            [ tr []
                [ th [] [ text "Person" ]
                , th [] [ text "Course" ]
                , th [] [ text "Preference Rank" ]
                ]
            ]
        , tbody []
            (assignment
                |> Dict.toList
                |> List.sortBy (\(person, (course, _)) -> (course, person))
                |> List.map viewAssignmentRow
            )
        ]


viewAssignmentRow : (String, (String, Int)) -> Html Msg
viewAssignmentRow (person, (course, rank)) =
    tr []
        [ td [] [ text person ]
        , td [] [ text course ]
        , td [] [ text (String.fromInt rank) ]
        ]



viewStyles : Html Msg
viewStyles =
    node "style" [] [ text """
        body {
            font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, "Helvetica Neue", Arial, sans-serif;
            line-height: 1.6;
            color: #333;
            background: #f5f5f5;
            margin: 0;
            padding: 0;
        }
        
        .container {
            max-width: 900px;
            margin: 0 auto;
            padding: 20px;
            background: white;
            min-height: 100vh;
        }
        
        .header {
            text-align: center;
            padding: 40px 0 20px;
            border-bottom: 2px solid #e0e0e0;
            margin-bottom: 30px;
        }
        
        .header h1 {
            margin: 0;
            font-size: 3em;
            color: #2c3e50;
        }
        
        .subtitle {
            color: #666;
            font-size: 1.1em;
            margin-top: 10px;
        }
        
        .subtitle a {
            color: #3498db;
            text-decoration: none;
        }
        
        .subtitle a:hover {
            text-decoration: underline;
        }
        
        .instructions {
            background: #f8f9fa;
            border-left: 4px solid #3498db;
            padding: 20px;
            margin: 30px 0;
            border-radius: 4px;
        }
        
        .instructions h2 {
            margin-top: 0;
            color: #2c3e50;
        }
        
        .instructions h3 {
            margin-top: 20px;
            margin-bottom: 10px;
            color: #2c3e50;
            font-size: 1.1em;
        }
        
        .instructions p {
            margin: 15px 0;
        }
        
        .example-code {
            background: #2c3e50;
            color: #ecf0f1;
            padding: 15px;
            border-radius: 6px;
            overflow-x: auto;
            margin: 15px 0;
        }
        
        .example-code code {
            font-family: 'Monaco', 'Menlo', 'Consolas', monospace;
            font-size: 0.9em;
            line-height: 1.5;
        }
        
        .inline-code {
            background: #e8f4f8;
            color: #2c3e50;
            padding: 2px 6px;
            border-radius: 3px;
            font-family: 'Monaco', 'Menlo', 'Consolas', monospace;
            font-size: 0.9em;
        }
        
        .tip {
            background: #fff8dc;
            border-left: 4px solid #f39c12;
            padding: 12px;
            margin: 15px 0;
            border-radius: 4px;
        }
        
        .instructions ol {
            margin: 15px 0;
        }
        
        .instructions li {
            margin: 8px 0;
        }
        
        .example-note {
            margin-top: 15px;
            font-style: italic;
            color: #666;
        }
        
        .example-note a {
            color: #3498db;
        }
        
        .upload-section {
            text-align: center;
            padding: 40px 0;
        }
        
        .upload-button {
            background: #3498db;
            color: white;
            border: none;
            padding: 15px 40px;
            font-size: 1.2em;
            border-radius: 6px;
            cursor: pointer;
            transition: background 0.3s;
        }
        
        .upload-button:hover {
            background: #2980b9;
        }
        
        .error {
            background: #fee;
            border: 1px solid #fcc;
            border-left: 4px solid #e74c3c;
            padding: 20px;
            margin: 30px 0;
            border-radius: 4px;
        }
        
        .error h3 {
            margin-top: 0;
            color: #c0392b;
        }
        
        .error p {
            color: #555;
            margin-bottom: 0;
        }
        
        .warnings {
            background: #fffbf0;
            border: 1px solid #ffeaa7;
            border-left: 4px solid #fdcb6e;
            padding: 20px;
            margin: 30px 0;
            border-radius: 4px;
        }
        
        .warnings h3 {
            margin-top: 0;
            color: #d68910;
        }
        
        .warnings ul {
            margin: 10px 0 0 0;
            padding-left: 20px;
        }
        
        .warnings li {
            color: #555;
            margin: 5px 0;
        }
        
        .results {
            margin-top: 40px;
        }
        
        .results-content {
            position: relative;
            background: #f8f9fa;
            border: 1px solid #d1d5da;
            border-radius: 6px;
            padding: 20px;
            margin-bottom: 20px;
        }
        
        .copy-container {
            position: absolute;
            top: 12px;
            right: 12px;
            display: flex;
            align-items: center;
            gap: 8px;
        }
        
        .copy-button {
            background: white;
            color: #586069;
            border: 1px solid #d1d5da;
            padding: 6px;
            border-radius: 6px;
            cursor: pointer;
            transition: all 0.2s;
            display: flex;
            align-items: center;
            justify-content: center;
        }
        
        .copy-button:hover {
            background: #f3f4f6;
            border-color: #9ca3af;
            color: #24292e;
        }
        
        .copied-message {
            color: #28a745;
            font-size: 14px;
            font-weight: 500;
            animation: fadeIn 0.2s ease-in;
        }
        
        @keyframes fadeIn {
            from { opacity: 0; }
            to { opacity: 1; }
        }
        
        .distribution {
            position: relative;
            background: #f8f9fa;
            border: 1px solid #d1d5da;
            padding: 20px;
            border-radius: 6px;
            margin-bottom: 20px;
            margin-top: 20px;
        }
        
        .distribution-header {
            display: flex;
            justify-content: space-between;
            align-items: center;
            margin-bottom: 12px;
        }
        
        .distribution-caption {
            margin: 0;
            font-size: 1.2em;
            font-weight: 600;
            color: #24292e;
        }
        
        .results-table {
            width: 100%;
            border-collapse: collapse;
            background: white;
            border-radius: 6px;
            overflow: hidden;
        }
        
        .results-table th,
        .results-table td {
            padding: 12px;
            text-align: left;
        }
        
        .results-table th {
            background: #34495e;
            color: white;
            font-weight: 600;
        }
        
        .results-table tbody tr {
            border-bottom: 1px solid #e1e4e8;
        }
        
        .results-table tbody tr:last-child {
            border-bottom: none;
        }
        
        .results-table tr:hover {
            background: #f6f8fa;
        }
        
        .download-section {
            text-align: center;
            margin-top: 30px;
        }
        
        .download-button {
            background: #27ae60;
            color: white;
            border: none;
            padding: 12px 30px;
            font-size: 1.1em;
            border-radius: 6px;
            cursor: pointer;
            transition: background 0.3s;
        }
        
        .download-button:hover {
            background: #229954;
        }
        
        .progress-container {
            text-align: center;
            padding: 40px 20px;
        }
        
        .progress-message {
            font-size: 1.3em;
            color: #2c3e50;
            margin-bottom: 20px;
            font-weight: 500;
        }
        
        .progress-bar-container {
            background: #e0e0e0;
            border-radius: 12px;
            height: 24px;
            overflow: hidden;
            margin: 20px auto;
            max-width: 400px;
            box-shadow: inset 0 2px 4px rgba(0, 0, 0, 0.1);
        }
        
        .progress-bar {
            background: linear-gradient(90deg, #3498db 0%, #2980b9 100%);
            height: 100%;
            border-radius: 12px;
            transition: width 0.3s ease;
            display: flex;
            align-items: center;
            justify-content: flex-end;
            padding-right: 10px;
            box-shadow: 0 2px 4px rgba(52, 152, 219, 0.3);
        }

        .progress-bar.indeterminate {
            width: 40%;
            background: linear-gradient(90deg, rgba(255,255,255,0.15), rgba(255,255,255,0.45), rgba(255,255,255,0.15));
            animation: indeterminate 1.2s infinite linear;
            transform: translateX(-150%);
        }

        @keyframes indeterminate {
            0% { transform: translateX(-150%); }
            50% { transform: translateX(50%); }
            100% { transform: translateX(150%); }
        }
        
        .progress-text {
            color: #2c3e50;
            font-size: 1.1em;
            margin-top: 10px;
            font-weight: 500;
        }
        
        .solution-count-header {
            background: #e7f3ff;
            border: 1px solid #b3d9ff;
            border-radius: 6px;
            padding: 12px 16px;
            margin-bottom: 20px;
            color: #0366d6;
            font-weight: 600;
            text-align: center;
        }
        
        .single-solution {
            margin-bottom: 40px;
        }
        
        .single-solution:not(:last-child) {
            padding-bottom: 40px;
            border-bottom: 3px solid #e1e4e8;
        }
        
        .solution-header {
            color: #0366d6;
            font-size: 1.5em;
            margin: 0 0 20px 0;
            padding: 12px 16px;
            background: #f6f8fa;
            border-left: 4px solid #0366d6;
            border-radius: 4px;
        }
    """ ]
