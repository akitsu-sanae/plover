module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Cvc4 exposing (..)
import Html exposing (Html, a, aside, br, button, div, h1, header, label, li, option, select, text, textarea, ul)
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
    = Z3Params Z3.Params
    | Cvc4Params Cvc4.Params


type alias Model =
    { params : Params
    , input : String
    , output : Maybe String
    , isLoading : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { params = Z3Params Z3.default
      , input = ""
      , output = Nothing
      , isLoading = False
      }
    , Cmd.none
    )


isActiveSolver : Solver -> Params -> Bool
isActiveSolver solver params =
    case ( params, solver ) of
        ( Z3Params _, Z3Solver ) ->
            True

        ( Cvc4Params _, Cvc4Solver ) ->
            True

        _ ->
            False



-- UPDATE


type Solver
    = Z3Solver
    | Cvc4Solver


solverOfStr : String -> Maybe Solver
solverOfStr str =
    case String.toLower str of
        "z3" ->
            Just Z3Solver

        "cvc4" ->
            Just Cvc4Solver

        _ ->
            Nothing


type UpdateParamMsg
    = UpdateZ3Param Z3.UpdateParamMsg
    | UpdateCvc4Param Cvc4.UpdateParamMsg


type Msg
    = ChangeSolver Solver
    | UpdateParam UpdateParamMsg
    | Input String
    | Verify
    | Output (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeSolver solver ->
            ( { model
                | params =
                    case solver of
                        Z3Solver ->
                            Z3Params Z3.default

                        Cvc4Solver ->
                            Cvc4Params Cvc4.default
              }
            , Cmd.none
            )

        UpdateParam paramMsg ->
            ( { model
                | params =
                    case ( model.params, paramMsg ) of
                        ( Z3Params params, UpdateZ3Param msg_ ) ->
                            Z3Params <| Z3.update msg_ params

                        ( Cvc4Params params, UpdateCvc4Param msg_ ) ->
                            Cvc4Params <| Cvc4.update msg_ params

                        ( _, _ ) ->
                            undefined ()
              }
            , Cmd.none
            )

        Input src ->
            ( { model | input = src }, Cmd.none )

        Verify ->
            ( { model | isLoading = True }, getVerificationResult model )

        Output output ->
            ( { model
                | isLoading = False
                , output =
                    case output of
                        Ok output_ ->
                            Just output_

                        Err err ->
                            Just <| toString err
              }
            , Cmd.none
            )



-- HTTP


createVerificationRequestBody : Model -> Http.Body
createVerificationRequestBody model =
    Http.jsonBody <|
        Json.Encode.object
            [ ( "src", Json.Encode.string model.input )
            , ( "argments"
              , case model.params of
                    Z3Params params ->
                        Z3.createJson params

                    Cvc4Params params ->
                        Cvc4.createJson params
              )
            ]


getVerificationResult : Model -> Cmd Msg
getVerificationResult model =
    Http.post
        { url = "https://qtafsl7jpf.execute-api.us-east-2.amazonaws.com/ProductionStage/verify"
        , body = createVerificationRequestBody model
        , expect = Http.expectJson Output resultDecoder
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
            [ div [ class "column col-4-mr-auto" ] [ createParamsUi model.params ]
            , div [ class "column col-8" ] [ createMainUi model.output model.isLoading ]
            ]
        ]


createChangeSolverUi : Params -> Html Msg
createChangeSolverUi params =
    let
        buttonClass solver =
            class <|
                if isActiveSolver solver params then
                    "btn btn-primary"

                else
                    "btn"
    in
    div [ class "btn-group btn-group-block" ]
        [ button
            [ buttonClass Z3Solver, onClick <| ChangeSolver Z3Solver ]
            [ a [ href "#" ] [ text "Z3" ] ]
        , button
            [ buttonClass Cvc4Solver, onClick <| ChangeSolver Cvc4Solver ]
            [ a [ href "#" ] [ text "CVC4" ] ]
        ]


createParamsUi : Params -> Html Msg
createParamsUi params =
    ul [ class "menu" ]
        [ li
            [ class "menu-item" ]
            [ createChangeSolverUi params ]
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
        Z3Params params_ ->
            Html.map (\msg -> UpdateParam <| UpdateZ3Param msg) <| Z3.createUi params_

        Cvc4Params params_ ->
            Html.map (\msg -> UpdateParam <| UpdateCvc4Param msg) <| Cvc4.createUi params_
