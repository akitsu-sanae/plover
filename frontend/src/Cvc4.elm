module Cvc4 exposing (Lang(..), Msg(..), Params, createJson, createUi, default, update)

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
    , seed : Maybe Int
    , cpuTime : Bool
    , incremental : Bool
    , resourceLimitPer : Maybe Int
    , resourceLimit : Maybe Int
    , timeLimitPer : Maybe Int
    , timeLimit : Maybe Int
    , approxBranchDepth : Maybe Int
    , arithNoPartialFun : Bool
    }


type Msg
    = Lang Lang
    | OutputLang OutputLang
    | Verbosity Int
    | Seed (Maybe Int)
    | CpuTime
    | Incremental
    | ResourceLimitPer (Maybe Int)
    | ResourceLimit (Maybe Int)
    | TimeLimitPer (Maybe Int)
    | TimeLimit (Maybe Int)
    | ApproxBranchDepth (Maybe Int)
    | ArithNoPartialFun


update : Msg -> Params -> Params
update msg params =
    case msg of
        Lang lang ->
            { params | lang = lang }

        OutputLang outputLang ->
            { params | outputLang = outputLang }

        Verbosity n ->
            { params | verbosity = n }

        Seed n ->
            { params | seed = n }

        CpuTime ->
            { params | cpuTime = not params.cpuTime }

        Incremental ->
            { params | incremental = not params.incremental }

        ResourceLimitPer limit ->
            { params | resourceLimitPer = limit }

        ResourceLimit limit ->
            { params | resourceLimit = limit }

        TimeLimitPer limit ->
            { params | timeLimitPer = limit }

        TimeLimit limit ->
            { params | timeLimit = limit }

        ApproxBranchDepth depth ->
            { params | approxBranchDepth = depth }

        ArithNoPartialFun ->
            { params | arithNoPartialFun = not params.arithNoPartialFun }


createJson : Params -> Json.Encode.Value
createJson params =
    Json.Encode.object
        [ ( "cvc4"
          , Json.Encode.object
                [ ( "lang", jsonOfLang params.lang )
                , ( "output lang", jsonOfOutputLang params.outputLang )
                , ( "verbosity", Json.Encode.int params.verbosity )
                , ( "cpu time", Json.Encode.bool params.cpuTime )
                , ( "incremental", Json.Encode.bool params.incremental )
                ]
          )
        ]


createUi : Params -> Html Msg
createUi params =
    div [ class "form-group" ]
        [ createSelectLine (List.map stringOfLang langs) (\str -> Lang <| unwrap <| langOfString str)
        , createSelectLine (List.map stringOfOutputLang outputLangs) (\str -> OutputLang <| unwrap <| outputLangOfString str)
        ]


default : Params
default =
    Params
        Auto
        OAuto
        0
        Nothing
        False
        False
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        False
