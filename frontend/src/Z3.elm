module Z3 exposing (Display, Format(..), Limit, Msg(..), Params, createJson, createUi, default, update)

import Dict exposing (Dict)
import Html exposing (Html, br, div, input, option, select, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Json.Encode
import UiUtil exposing (..)
import Util exposing (..)


type Format
    = Smtlib2
    | Datalog
    | Dimacs
    | WeightedCnfDimacs
    | PbOptimization
    | CplexLp
    | Z3log


formats : List Format
formats =
    [ Smtlib2
    , Datalog
    , Dimacs
    , WeightedCnfDimacs
    , PbOptimization
    , CplexLp
    , Z3log
    ]


formatString : List ( Format, String )
formatString =
    [ ( Smtlib2, "smtlib2" )
    , ( Datalog, "datalog" )
    , ( Dimacs, "DIMACS" )
    , ( WeightedCnfDimacs, "Weighted CNF DIMACS" )
    , ( PbOptimization, "PB optimization" )
    , ( CplexLp, "CPLEX LP" )
    , ( Z3log, "Z3 log" )
    ]


stringOfFormat : Format -> String
stringOfFormat fmt =
    unwrap <|
        List.foldl
            (\( f, str ) acc ->
                case acc of
                    Just _ ->
                        acc

                    Nothing ->
                        if fmt == f then
                            Just str

                        else
                            Nothing
            )
            Nothing
            formatString


formatOfString : String -> Maybe Format
formatOfString str =
    List.foldl
        (\( fmt, s ) acc ->
            case acc of
                Just _ ->
                    acc

                Nothing ->
                    if str == s then
                        Just fmt

                    else
                        Nothing
        )
        Nothing
        formatString


jsonOfFormat : Format -> Json.Encode.Value
jsonOfFormat format =
    Json.Encode.string <| stringOfFormat format


type alias Display =
    { globalParams : Bool
    , globalParamDescs : Bool
    , statistics : Bool
    , warnings : Bool
    }


type alias Limit =
    { timeout : Maybe Int
    , softTimeout : Maybe Int
    , memory : Maybe Int
    }


type alias Params =
    { format : Format
    , display : Display
    , limit : Limit
    , globalParams : Dict String String
    , moduleParams : Dict ( String, String ) String
    }


type Msg
    = Format Format
    | DisplayGlobalParams
    | DisplayGlobalParamDescs
    | DisplayStatistics
    | DisplayWarnings
    | Timeout (Maybe Int)
    | SoftTimeout (Maybe Int)
    | Memory (Maybe Int)


update : Msg -> Params -> Params
update msg params =
    case msg of
        Format format ->
            { params | format = format }

        DisplayGlobalParams ->
            let
                globalParams =
                    not params.display.globalParams

                oldDisplay =
                    params.display
            in
            { params | display = { oldDisplay | globalParams = globalParams } }

        DisplayGlobalParamDescs ->
            let
                globalParamDescs =
                    not params.display.globalParamDescs

                oldDisplay =
                    params.display
            in
            { params | display = { oldDisplay | globalParamDescs = globalParamDescs } }

        DisplayStatistics ->
            let
                statistics =
                    not params.display.statistics

                oldDisplay =
                    params.display
            in
            { params | display = { oldDisplay | statistics = statistics } }

        DisplayWarnings ->
            let
                warnings =
                    not params.display.warnings

                oldDisplay =
                    params.display
            in
            { params | display = { oldDisplay | warnings = warnings } }

        Timeout n ->
            let
                oldLimit =
                    params.limit
            in
            { params | limit = { oldLimit | timeout = n } }

        SoftTimeout n ->
            let
                oldLimit =
                    params.limit
            in
            { params | limit = { oldLimit | softTimeout = n } }

        Memory n ->
            let
                oldLimit =
                    params.limit
            in
            { params | limit = { oldLimit | memory = n } }


createJson : Params -> Json.Encode.Value
createJson params =
    Json.Encode.object
        [ ( "z3"
          , Json.Encode.object
                [ ( "format", jsonOfFormat params.format )
                , ( "display"
                  , Json.Encode.object
                        [ ( "global_parameters", Json.Encode.bool params.display.globalParams )
                        , ( "global_parameter_descriptions", Json.Encode.bool params.display.globalParamDescs )
                        , ( "statistics", Json.Encode.bool params.display.statistics )
                        , ( "warnings", Json.Encode.bool params.display.warnings )
                        ]
                  )
                , ( "limit"
                  , Json.Encode.object
                        (jsonOfMaybeInt "timeout" params.limit.timeout
                            ++ jsonOfMaybeInt "soft timeout" params.limit.softTimeout
                            ++ jsonOfMaybeInt "memory" params.limit.memory
                        )
                  )
                , ( "global_parameters", Json.Encode.object (Dict.values <| Dict.map (\name value -> ( name, Json.Encode.string value )) params.globalParams) )
                , ( "module_parameters", Json.Encode.object (Dict.values <| Dict.map (\( moduleName, paramName ) value -> ( moduleName ++ "." ++ paramName, Json.Encode.string value )) params.moduleParams) )
                ]
          )
        ]


createUi : Params -> Html Msg
createUi params =
    div [ class "form-group" ] <|
        [ createSelectLine (List.map stringOfFormat formats) (\str -> Format <| unwrap <| formatOfString str) "format"
        , createCheckboxLine DisplayGlobalParams "display global parameters"
        , createCheckboxLine DisplayGlobalParamDescs "display global parameter descriptions"
        , createCheckboxLine DisplayStatistics "display statistics"
        , createCheckboxLine DisplayWarnings "display warnings"
        ]
            ++ createMaybeIntInput Timeout "timeout" params.limit.timeout
            ++ createMaybeIntInput SoftTimeout "soft timeout" params.limit.softTimeout
            ++ createMaybeIntInput Memory "memory" params.limit.memory


default : Params
default =
    Params
        Smtlib2
        (Display False False False False)
        (Limit Nothing Nothing Nothing)
        (Dict.fromList [])
        (Dict.fromList [])
