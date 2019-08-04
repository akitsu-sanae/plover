module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, br, button, div, option, select, text, textarea)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }



-- UTIL


undefined : () -> a
undefined _ =
    Debug.todo "<undefined>"



-- MODEL


type alias Model =
    { solver : String
    , format : String
    , input : String
    , output : Maybe String
    }


init : Model
init =
    Model "" "" "" Nothing



-- UPDATE


type Msg
    = Solver String
    | Format String
    | Input String
    | Output String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Solver solver ->
            { model | solver = solver }

        Format format ->
            { model | format = format }

        Input src ->
            { model | input = src }

        Output output ->
            { model | output = Just output }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ select [ onInput Solver ]
            [ option [ value "Z3" ] [ text "z3" ]
            ]
        , select [ onInput Format ]
            [ option [ value "Smtlib2" ] [ text "smtlib 2.6" ] ]
        , br [] []
        , textarea [ cols 40, rows 10, placeholder "...", onInput Input ] []
        , br [] []
        , button [] [ text "verify!" ]
        , br [] []
        , textarea [ cols 40, rows 10, placeholder "output", onInput Output ] []
        ]
