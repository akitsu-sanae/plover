module Cvc4 exposing (Lang(..), Params, UpdateParamMsg(..), createJson, createUi, default, update)

import Dict exposing (Dict)
import Html exposing (Html, br, div, input, option, select, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Json.Encode
import UiUtil exposing (..)
import Util exposing (..)


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
    unwrap <|
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
    unwrap <|
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


type UpdateParamMsg
    = Lang Lang
    | OutputLang OutputLang
    | Verbosity Int
    | Stats
    | Seed (Maybe Int)
    | StrictParsing
    | CpuTime
    | HardLimit
    | Incremental
    | ProduceAssertions
    | ProduceModels
    | ResourceLimitPer (Maybe Int)
    | ResourceLimit (Maybe Int)
    | TimeLimitPer (Maybe Int)
    | TimeLimit (Maybe Int)


update : UpdateParamMsg -> Params -> Params
update msg params =
    case msg of
        Lang lang ->
            { params | lang = lang }

        OutputLang outputLang ->
            { params | outputLang = outputLang }

        Verbosity n ->
            { params | verbosity = n }

        Stats ->
            { params | stats = not params.stats }

        Seed n ->
            { params | seed = n }

        StrictParsing ->
            { params | strictParsing = not params.strictParsing }

        CpuTime ->
            { params | cpuTime = not params.cpuTime }

        HardLimit ->
            { params | cpuTime = not params.hardLimit }

        Incremental ->
            { params | incremental = not params.incremental }

        ProduceAssertions ->
            { params | produceAssertions = not params.produceAssertions }

        ProduceModels ->
            { params | produceModels = not params.produceModels }

        ResourceLimitPer limit ->
            { params | resourceLimitPer = limit }

        ResourceLimit limit ->
            { params | resourceLimit = limit }

        TimeLimitPer limit ->
            { params | timeLimitPer = limit }

        TimeLimit limit ->
            { params | timeLimit = limit }


jsonFromMaybeInt : String -> Maybe Int -> List ( String, Json.Encode.Value )
jsonFromMaybeInt label x =
    case x of
        Just n ->
            [ ( label, Json.Encode.int n ) ]

        Nothing ->
            []


createJson : Params -> Json.Encode.Value
createJson params =
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
                ]
                    ++ jsonFromMaybeInt "seed" params.seed
                    ++ jsonFromMaybeInt "rlimit-per" params.resourceLimitPer
                    ++ jsonFromMaybeInt "rlimit" params.resourceLimit
                    ++ jsonFromMaybeInt "tlimit-per" params.timeLimitPer
                    ++ jsonFromMaybeInt "tlimit" params.timeLimit
                    ++ [ ( "others", Json.Encode.list Json.Encode.int [] ) ]
            -- TODO
          )
        ]


createUi : Params -> Html UpdateParamMsg
createUi params =
    div [ class "form-group" ] <|
        [ createSelectLine (List.map stringOfLang langs) (\str -> Lang <| unwrap <| langOfString str) "lang"
        , createSelectLine (List.map stringOfOutputLang outputLangs) (\str -> OutputLang <| unwrap <| outputLangOfString str) "output lang"
        , createNumberLine Verbosity "verbosity"
        , createCheckboxLine Stats "stats"
        , createCheckboxLine StrictParsing "strict parsing"
        , createCheckboxLine CpuTime "cpu time"
        , createCheckboxLine HardLimit "hard limit"
        , createCheckboxLine Incremental "incremental"
        , createCheckboxLine ProduceAssertions "produce assertions"
        , createCheckboxLine ProduceModels "produce models"
        ]
            ++ createMaybeIntInput Seed "seed" params.seed
            ++ createMaybeIntInput ResourceLimitPer "resource limit per" params.resourceLimitPer
            ++ createMaybeIntInput ResourceLimit "resource limit" params.resourceLimit
            ++ createMaybeIntInput TimeLimitPer "time limit per" params.timeLimitPer
            ++ createMaybeIntInput TimeLimit "time limit" params.timeLimit


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
