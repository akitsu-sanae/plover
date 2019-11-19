module CVC4.Model exposing (Lang, OutputLang, Params, default, jsonOfParams, langOfString, langs, outputLangOfString, outputLangs, stringOfLang, stringOfOutputLang)

import Json.Encode
import Util


type Lang
    = Auto
    | Cvc4
    | Smtlib1
    | Smtlib2
    | Smtlib25
    | Smtlib26
    | Smtlib261
    | Tptp
    | Sygus


langs : List Lang
langs =
    [ Auto
    , Cvc4
    , Smtlib1
    , Smtlib2
    , Smtlib25
    , Smtlib26
    , Smtlib261
    , Tptp
    , Sygus
    ]


langString : List ( Lang, String )
langString =
    [ ( Auto, "auto" )
    , ( Cvc4, "cvc4" )
    , ( Smtlib1, "smtlib1" )
    , ( Smtlib2, "smtlib2.0" )
    , ( Smtlib25, "smtlib2.5" )
    , ( Smtlib26, "smtlib2.6" )
    , ( Smtlib261, "smtlib2.6.1" )
    , ( Tptp, "TPTP" )
    , ( Sygus, "SyGuS" )
    ]


stringOfLang : Lang -> String
stringOfLang lang =
    Util.unwrap <|
        List.foldl
            (\( l, str ) acc ->
                case acc of
                    Just _ ->
                        acc

                    Nothing ->
                        if lang == l then
                            Just str

                        else
                            Nothing
            )
            Nothing
            langString


langOfString : String -> Maybe Lang
langOfString str =
    List.foldl
        (\( lang, s ) acc ->
            case acc of
                Just _ ->
                    acc

                Nothing ->
                    if str == s then
                        Just lang

                    else
                        Nothing
        )
        Nothing
        langString


jsonOfLang : Lang -> Json.Encode.Value
jsonOfLang lang =
    Json.Encode.string <| stringOfLang lang


type OutputLang
    = OAuto
    | OCvc4
    | OCvc3
    | OSmtlib2
    | OSmtlib25
    | OSmtlib26
    | OSmtlib261
    | OTptp
    | OZ3str
    | OAst


outputLangs : List OutputLang
outputLangs =
    [ OAuto
    , OCvc4
    , OCvc3
    , OSmtlib2
    , OSmtlib25
    , OSmtlib26
    , OSmtlib261
    , OTptp
    , OZ3str
    , OAst
    ]


outputLangString : List ( OutputLang, String )
outputLangString =
    [ ( OAuto, "auto" )
    , ( OCvc4, "cvc4" )
    , ( OCvc3, "cvc3" )
    , ( OSmtlib2, "smtlib2.0" )
    , ( OSmtlib25, "smtlib2.5" )
    , ( OSmtlib26, "smtlib2.6" )
    , ( OSmtlib261, "smtlib2.6.1" )
    , ( OTptp, "TPTP" )
    , ( OZ3str, "Z3 str" )
    , ( OAst, "Ast" )
    ]


stringOfOutputLang : OutputLang -> String
stringOfOutputLang outputLang =
    Util.unwrap <|
        List.foldl
            (\( ol, str ) acc ->
                case acc of
                    Just _ ->
                        acc

                    Nothing ->
                        if outputLang == ol then
                            Just str

                        else
                            Nothing
            )
            Nothing
            outputLangString


outputLangOfString : String -> Maybe OutputLang
outputLangOfString str =
    List.foldl
        (\( outputLang, s ) acc ->
            case acc of
                Just _ ->
                    acc

                Nothing ->
                    if str == s then
                        Just outputLang

                    else
                        Nothing
        )
        Nothing
        outputLangString


jsonOfOutputLang : OutputLang -> Json.Encode.Value
jsonOfOutputLang outputLang =
    Json.Encode.string <| stringOfOutputLang outputLang


type alias Params =
    { lang : Lang
    , outputLang : OutputLang
    , verbosity : Int
    , stats : Bool
    , seed : Maybe Int
    , strictParsing : Bool
    , cpuTime : Bool
    , hardLimit : Bool
    , incremental : Bool
    , produceAssertions : Bool
    , produceModels : Bool
    , resourceLimitPer : Maybe Int
    , resourceLimit : Maybe Int
    , timeLimitPer : Maybe Int
    , timeLimit : Maybe Int
    , others : List String
    }


default : Params
default =
    { lang = Auto
    , outputLang = OAuto
    , verbosity = 0
    , stats = False
    , seed = Nothing
    , strictParsing = False
    , cpuTime = False
    , hardLimit = False
    , incremental = False
    , produceAssertions = False
    , produceModels = False
    , resourceLimitPer = Nothing
    , resourceLimit = Nothing
    , timeLimitPer = Nothing
    , timeLimit = Nothing
    , others = []
    }


jsonOfParams : Params -> Json.Encode.Value
jsonOfParams params =
    Json.Encode.object
        [ ( "cvc4"
          , Json.Encode.object <|
                [ ( "lang", jsonOfLang params.lang )
                , ( "output-lang", jsonOfOutputLang params.outputLang )
                , ( "verbosity", Json.Encode.int params.verbosity )
                , ( "stats", Json.Encode.bool params.stats )
                , ( "strict-parsing", Json.Encode.bool params.strictParsing )
                , ( "cpu-time", Json.Encode.bool params.cpuTime )
                , ( "hard-limit", Json.Encode.bool params.hardLimit )
                , ( "incremental", Json.Encode.bool params.incremental )
                , ( "produce-assertions", Json.Encode.bool params.produceAssertions )
                , ( "produce-models", Json.Encode.bool params.produceModels )
                , ( "others", Json.Encode.list Json.Encode.int [] ) -- TODO
                ]
                    ++ Util.zip [ "seed", "rlimit-per", "rlimit", "tlimit-per", "tlimit" ]
                        (List.filterMap (Maybe.map (\x -> Json.Encode.string <| String.fromInt x))
                            [ params.seed, params.resourceLimitPer, params.resourceLimit, params.timeLimitPer, params.timeLimit ]
                        )
          )
        ]
