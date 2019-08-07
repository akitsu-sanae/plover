module Cvc4 exposing (Format(..), Msg(..), Params, createJson, createUi, default, update)

import Dict exposing (Dict)
import Html exposing (Html, br, div, input, option, select, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Json.Encode
import Util exposing (..)


type Format
    = Smtlib2


type alias Params =
    { format : Format
    }


type Msg
    = Format Format


update : Msg -> Params -> Params
update msg params =
    case msg of
        Format format ->
            { params | format = format }


createJson : Params -> Json.Encode.Value
createJson params =
    Json.Encode.object
        [ ( "cvc4"
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
        ]


default : Params
default =
    Params
        Smtlib2
