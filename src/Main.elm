port module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import File exposing (File)
import File.Select as Select
import Task
import Process
import Svg exposing (svg)
import Svg.Attributes as SvgAttr
import Matching exposing (Results, AssignmentRecord, DistributionRecord, State)
import CsvParser
import Validate
import Random


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
    | ShowingResults Results (List String) CopyState


type alias CopyState =
    { assignmentsCopied : Bool
    , summaryCopied : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( NoResults, Cmd.none )


-- UPDATE
type Msg
    = UploadRequested
    | FileSelected File
    | FileLoaded String
    | SolveCompleted Results (List String)
    | CopyToClipboard
    | CopySummaryToClipboard
    | CopiedMessageHide
    | SummaryCopiedMessageHide


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
                        generator = Matching.solve validated.data
                    in
                    ( model
                    , Random.generate (\results -> SolveCompleted results validated.warnings) generator
                    )
                
                Err errorMsg ->
                    ( ShowingError errorMsg, Cmd.none )
        
        SolveCompleted results warnings ->
            ( ShowingResults results warnings { assignmentsCopied = False, summaryCopied = False }
            , Cmd.none
            )
        
        CopyToClipboard ->
            case model of
                ShowingResults results warnings copyState ->
                    ( ShowingResults results warnings { copyState | assignmentsCopied = True }
                    , Cmd.batch
                        [ copyToClipboard (formatAssignmentsForCopy results.assignments)
                        , Process.sleep 2000 |> Task.perform (\_ -> CopiedMessageHide)
                        ]
                    )
                
                NoResults ->
                    ( model, Cmd.none )
                
                ShowingError _ ->
                    ( model, Cmd.none )
        
        CopySummaryToClipboard ->
            case model of
                ShowingResults results warnings copyState ->
                    ( ShowingResults results warnings { copyState | summaryCopied = True }
                    , Cmd.batch
                        [ copyToClipboard (formatDistributionForCopy results.distribution)
                        , Process.sleep 2000 |> Task.perform (\_ -> SummaryCopiedMessageHide)
                        ]
                    )
                
                NoResults ->
                    ( model, Cmd.none )
                
                ShowingError _ ->
                    ( model, Cmd.none )
        
        CopiedMessageHide ->
            case model of
                ShowingResults results warnings copyState ->
                    ( ShowingResults results warnings { copyState | assignmentsCopied = False }, Cmd.none )
                
                NoResults ->
                    ( model, Cmd.none )
                
                ShowingError _ ->
                    ( model, Cmd.none )
        
        SummaryCopiedMessageHide ->
            case model of
                ShowingResults results warnings copyState ->
                    ( ShowingResults results warnings { copyState | summaryCopied = False }, Cmd.none )
                
                NoResults ->
                    ( model, Cmd.none )
                
                ShowingError _ ->
                    ( model, Cmd.none )


formatAssignmentsForCopy : List AssignmentRecord -> String
formatAssignmentsForCopy assignments =
    let
        header = "Course,Person,Preference Rank"
        rows = List.map formatAssignmentRow assignments
        formatAssignmentRow a = a.course ++ "," ++ a.person ++ "," ++ 
            (case a.rank of
                Just r -> String.fromInt r
                Nothing -> "N/A"
            )
    in
    String.join "\n" (header :: rows)


formatDistributionForCopy : List DistributionRecord -> String
formatDistributionForCopy distribution =
    let
        formatDistRow d = 
            ordinal d.rank ++ " choice: " ++ String.fromInt d.count ++ " " ++ pluralize d.count "person" "people"
    in
    String.join "\n" (List.map formatDistRow distribution)


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
subscriptions _ =
    Sub.none


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
                , text "Person A,        *1,       2,        -\n"
                , text "Person B,        2,        1,        3\n"
                , text "Person C,        -,        2,        1\n"
                , text "Person D,        1,        3,        2"
                ]
            ]
        , ul []
            [ li [] [ text "Person A is fixed to Course 1 and cannot be assigned to Course 3" ]
            , li [] [ text "Person C cannot be assigned to Course 1" ]
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
        
        ShowingResults results warnings copyState ->
            div [ class "results" ]
                [ viewWarnings warnings
                , div [ class "results-content" ]
                    [ div [ class "distribution-header" ]
                        [ h3 [ class "distribution-caption" ] [ text "Assignments" ]
                        , div [ class "copy-container" ]
                            [ if copyState.assignmentsCopied then
                                span [ class "copied-message" ] [ text "Copied!" ]
                              else
                                text ""
                            , button [ class "copy-button", onClick CopyToClipboard, title "Copy to clipboard" ]
                                [ svg [ SvgAttr.width "16", SvgAttr.height "16", SvgAttr.viewBox "0 0 16 16", SvgAttr.fill "currentColor" ]
                                    [ Svg.path [ SvgAttr.d "M0 6.75C0 5.784.784 5 1.75 5h1.5a.75.75 0 0 1 0 1.5h-1.5a.25.25 0 0 0-.25.25v7.5c0 .138.112.25.25.25h7.5a.25.25 0 0 0 .25-.25v-1.5a.75.75 0 0 1 1.5 0v1.5A1.75 1.75 0 0 1 9.25 16h-7.5A1.75 1.75 0 0 1 0 14.25Z" ] []
                                    , Svg.path [ SvgAttr.d "M5 1.75C5 .784 5.784 0 6.75 0h7.5C15.216 0 16 .784 16 1.75v7.5A1.75 1.75 0 0 1 14.25 11h-7.5A1.75 1.75 0 0 1 5 9.25Zm1.75-.25a.25.25 0 0 0-.25.25v7.5c0 .138.112.25.25.25h7.5a.25.25 0 0 0 .25-.25v-7.5a.25.25 0 0 0-.25-.25Z" ] []
                                    ]
                                ]
                            ]
                        ]
                    , viewAssignmentsTable results.assignments
                    ]
                , viewDistribution results.distribution copyState.summaryCopied
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


viewDistribution : List DistributionRecord -> Bool -> Html Msg
viewDistribution dist showCopied =
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
                (List.map viewDistributionItem dist)
            ]
        ]


viewDistributionItem : DistributionRecord -> Html Msg
viewDistributionItem d =
    tr []
        [ td [] [ text (ordinal d.rank) ]
        , td [] [ text (String.fromInt d.count) ]
        ]


pluralize : Int -> String -> String -> String
pluralize n singular plural =
    if n == 1 then singular else plural


viewAssignmentsTable : List AssignmentRecord -> Html Msg
viewAssignmentsTable assignments =
    table [ class "results-table" ]
        [ thead []
            [ tr []
                [ th [] [ text "Course" ]
                , th [] [ text "Person" ]
                , th [] [ text "Preference Rank" ]
                ]
            ]
        , tbody []
            (List.map viewAssignmentRow assignments)
        ]


viewAssignmentRow : AssignmentRecord -> Html Msg
viewAssignmentRow assignment =
    tr []
        [ td [] [ text assignment.course ]
        , td [] [ text assignment.person ]
        , td [] [ text (case assignment.rank of
                            Just r -> String.fromInt r
                            Nothing -> "N/A"
                        ) ]
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
        
        .progress-text {
            color: #2c3e50;
            font-size: 1.1em;
            margin-top: 10px;
            font-weight: 500;
        }
    """ ]
