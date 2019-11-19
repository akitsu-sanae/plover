module Z3.Model exposing (Display, Format(..), Limit, Params, default, formatOfString, formats, jsonOfParams, stringOfFormat)

import Dict exposing (Dict)
import Json.Encode
import Util


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
    Util.unwrap <|
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
    , others : { currentInput : String, options : List String }
    }


default : Params
default =
    { format = Smtlib2
    , display =
        { globalParams = False
        , globalParamDescs = False
        , statistics = False
        , warnings = False
        }
    , limit =
        { timeout = Nothing
        , softTimeout = Nothing
        , memory = Nothing
        }
    , globalParams = Dict.fromList []
    , moduleParams = Dict.fromList []
    , others =
        { currentInput = ""
        , options = []
        }
    }


jsonOfParams : Params -> Json.Encode.Value
jsonOfParams params =
    Json.Encode.object
        [ ( "z3"
          , Json.Encode.object
                [ ( "format", Json.Encode.string <| stringOfFormat params.format )
                , ( "display"
                  , Json.Encode.object
                        [ ( "global-parameters", Json.Encode.bool params.display.globalParams )
                        , ( "global-parameter-descriptions", Json.Encode.bool params.display.globalParamDescs )
                        , ( "statistics", Json.Encode.bool params.display.statistics )
                        , ( "warnings", Json.Encode.bool params.display.warnings )
                        ]
                  )
                , ( "limit"
                  , Json.Encode.object <|
                        Util.zip
                            [ "timeout", "soft-timeout", "memory-limit" ]
                        <|
                            List.filterMap
                                (Maybe.map (\x -> Json.Encode.string <| String.fromInt x))
                                [ params.limit.timeout, params.limit.softTimeout, params.limit.memory ]
                  )
                , ( "global-parameters", Json.Encode.object (Dict.values <| Dict.map (\name value -> ( name, Json.Encode.string value )) params.globalParams) )
                , ( "module-parameters", Json.Encode.object (Dict.values <| Dict.map (\( moduleName, paramName ) value -> ( moduleName ++ "." ++ paramName, Json.Encode.string value )) params.moduleParams) )
                , ( "others", Json.Encode.list Json.Encode.string params.others.options )
                ]
          )
        ]
