module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Cvc4 exposing (..)
import Html exposing (Html, aside, br, button, div, h1, header, label, li, option, select, text, textarea, ul)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode exposing (Decoder, field, string)
import Json.Encode
import UiUtil exposing (..)
import Util exposing (..)
import Z3 exposing (..)


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type Params
    = Z3 Z3.Params
    | Cvc4 Cvc4.Params


type alias Model =
    { params : Params
    , input : String
    , output : Maybe String
    , isLoading : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model (Z3 Z3.default) "" Nothing False, Cmd.none )



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
            ( { model | isLoading = True }, getVerificationResult model )

        GotResult result ->
            case result of
                Ok json ->
                    ( { model | output = Just json, isLoading = False }, Cmd.none )

                Err err ->
                    ( { model | output = Just <| toString err, isLoading = False }, Cmd.none )



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
    div [ class "container" ]
        [ header [] [ h1 [] [ text "Plover" ] ]
        , div [ class "columns" ]
            [ div [ class "column col-3" ] [ createParamsUi model.params ]
            , div [ class "column col-8" ] [ createMainUi model.output model.isLoading ]
            ]
        ]


createParamsUi : Params -> Html Msg
createParamsUi params =
    ul [ class "menu" ]
        [ li [ class "menu-item" ] [ createSelectLine [ "z3", "cvc4" ] Solver "solver" ]
        , li [ class "divider" ] []
        , li [ class "menu-item" ] [ createSolverParamsUi params ]
        ]


createMainUi : Maybe String -> Bool -> Html Msg
createMainUi output isLoading =
    div [ class "form-group" ] <|
        [ label [ class "form-label", for "input-area" ] [ text "Query to Solver:" ]
        , textarea [ class "input-area form-input", onInput Input ] []
        , br [] []
        , button
            [ class
                (if isLoading then
                    "btn loading"

                 else
                    "btn"
                )
            , onClick Verify
            ]
            [ text "verify!" ]
        ]
            ++ (case output of
                    Nothing ->
                        []

                    Just out ->
                        [ br [] []
                        , label [ class "form-label", for "output-area" ] [ text "Response from Solver:" ]
                        , textarea [ class "output-area form-input" ] [ text out ]
                        ]
               )


createSolverParamsUi : Params -> Html Msg
createSolverParamsUi params =
    case params of
        Z3 z3params ->
            Html.map (\msg -> Z3Msg msg) <| Z3.createUi z3params

        Cvc4 cvc4params ->
            Html.map (\msg -> Cvc4Msg msg) <| Cvc4.createUi cvc4params
