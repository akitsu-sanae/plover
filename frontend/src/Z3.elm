module Z3 exposing (Display, Format(..), Limit, Msg(..), Params, createJson, createUi, default, update)

import Dict exposing (Dict)
import Html exposing (Html, br, div, input, option, select, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Json.Encode
import Util exposing (..)


type Format
    = Smtlib2
    | Datalog
    | Dimacs
    | WeightedCnfDimacs
    | PbOptimization
    | CplexLp
    | Z3log


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
    | Timeout Int
    | SoftTimeout Int
    | Memory Int


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

                display =
                    { oldDisplay | globalParams = globalParams }
            in
            { params | display = display }

        DisplayGlobalParamDescs ->
            let
                globalParamDescs =
                    not params.display.globalParamDescs

                oldDisplay =
                    params.display

                display =
                    { oldDisplay | globalParamDescs = globalParamDescs }
            in
            { params | display = display }

        DisplayStatistics ->
            let
                statistics =
                    not params.display.statistics

                oldDisplay =
                    params.display

                display =
                    { oldDisplay | statistics = statistics }
            in
            { params | display = display }

        DisplayWarnings ->
            let
                warnings =
                    not params.display.warnings

                oldDisplay =
                    params.display

                display =
                    { oldDisplay | warnings = warnings }
            in
            { params | display = display }

        Timeout n ->
            let
                oldLimit =
                    params.limit

                limit =
                    { oldLimit | timeout = Just n }
            in
            { params | limit = limit }

        SoftTimeout n ->
            let
                oldLimit =
                    params.limit

                limit =
                    { oldLimit | softTimeout = Just n }
            in
            { params | limit = limit }

        Memory n ->
            let
                oldLimit =
                    params.limit

                limit =
                    { oldLimit | memory = Just n }
            in
            { params | limit = limit }


createJson : Params -> Json.Encode.Value
createJson params =
    Json.Encode.object
        [ ( "z3"
          , Json.Encode.object
                []
          )
        ]


formatOfString : String -> Msg
formatOfString str =
    case str of
        "smtlib2" ->
            Format Smtlib2

        _ ->
            undefined ()


createUi : Params -> Html Msg
createUi params =
    div []
        [ select [ onInput formatOfString ]
            [ option [ value "smtlib2" ] [ text "smtlib2" ]
            ]
        , input [ type_ "checkbox", onClick DisplayGlobalParams ] [ text "display global parameters" ]
        , input [ type_ "checkbox", onClick DisplayGlobalParamDescs ] [ text "display global parameter descriptions" ]
        , input [ type_ "checkbox", onClick DisplayStatistics ] [ text "display statistics" ]
        , input [ type_ "checkbox", onClick DisplayWarnings ] [ text "display warnings" ]
        ]


default : Params
default =
    Params
        Smtlib2
        (Display False False False False)
        (Limit Nothing Nothing Nothing)
        (Dict.fromList [])
        (Dict.fromList [])
