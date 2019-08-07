module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Cvc4 exposing (..)
import Html exposing (Html, br, button, div, option, select, text, textarea)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode exposing (Decoder, field, string)
import Json.Encode
import Util exposing (..)
import Z3 exposing (..)


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


stringfyOutput : Maybe String -> String
stringfyOutput x =
    case x of
        Just str ->
            str

        Nothing ->
            ""



-- MODEL


type Params
    = Z3 Z3.Params
    | Cvc4 Cvc4.Params


type alias Model =
    { params : Params
    , input : String
    , output : Maybe String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model (Z3 Z3.default) "" Nothing, Cmd.none )



-- UPDATE


type Msg
    = Solver String
    | Z3Msg Z3.Msg
    | Cvc4Msg Cvc4.Msg
    | Input String
    | Verify
    | GotResult (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Solver solver ->
            case solver of
                "z3" ->
                    ( { model | params = Z3 Z3.default }, Cmd.none )

                "cvc4" ->
                    ( { model | params = Cvc4 Cvc4.default }, Cmd.none )

                _ ->
                    undefined ()

        Z3Msg z3msg ->
            case model.params of
                Z3 z3params ->
                    ( { model | params = Z3 <| Z3.update z3msg z3params }, Cmd.none )

                Cvc4 _ ->
                    undefined ()

        Cvc4Msg cvc4msg ->
            case model.params of
                Cvc4 cvc4params ->
                    ( { model | params = Cvc4 <| Cvc4.update cvc4msg cvc4params }, Cmd.none )

                Z3 _ ->
                    undefined ()

        Input src ->
            ( { model | input = src }, Cmd.none )

        Verify ->
            ( model, getVerificationResult model )

        GotResult result ->
            case result of
                Ok json ->
                    ( { model | output = Just json }, Cmd.none )

                Err err ->
                    ( { model | output = Just <| toString err }, Cmd.none )



-- HTTP


createVerificationRequestBody : Model -> Http.Body
createVerificationRequestBody model =
    Http.jsonBody <|
        Json.Encode.object
            [ ( "src", Json.Encode.string model.input )
            , ( "argments"
              , case model.params of
                    Z3 z3params ->
                        Z3.createJson z3params

                    Cvc4 cvc4params ->
                        Cvc4.createJson cvc4params
              )
            ]


getVerificationResult : Model -> Cmd Msg
getVerificationResult model =
    Http.post
        { url = "https://qtafsl7jpf.execute-api.us-east-2.amazonaws.com/ProductionStage/verify"
        , body = createVerificationRequestBody model
        , expect = Http.expectJson GotResult resultDecoder
        }


resultDecoder : Decoder String
resultDecoder =
    field "stdout" string



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ select [ onChange Solver ]
            [ option [ value "z3" ] [ text "z3" ]
            , option [ value "cvc4" ] [ text "cvc4" ]
            ]
        , createParamsUi model.params
        , br [] []
        , textarea [ cols 40, rows 10, placeholder "...", onInput Input ] []
        , br [] []
        , button [ onClick Verify ] [ text "verify!" ]
        , br [] []
        , textarea [ cols 40, rows 10, placeholder "output" ] [ text <| stringfyOutput model.output ]
        ]


createParamsUi : Params -> Html Msg
createParamsUi params =
    case params of
        Z3 z3params ->
            Html.map (\msg -> Z3Msg msg) <| Z3.createUi z3params

        Cvc4 cvc4params ->
            Html.map (\msg -> Cvc4Msg msg) <| Cvc4.createUi cvc4params
