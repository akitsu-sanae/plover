module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import CVC4.Model
import CVC4.Update
import CVC4.View
import Html exposing (Html, a, aside, br, button, div, h1, header, label, li, option, select, text, textarea, ul)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode exposing (Decoder, field, string)
import Json.Encode
import Util exposing (..)
import Z3.Model
import Z3.Update
import Z3.View


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type Params
    = Z3Params Z3.Model.Params
    | Cvc4Params CVC4.Model.Params


type alias History =
    { isSuccess : Bool, content : String }


type alias QueryResult =
    { histories : List History, focused : Int }


type alias Model =
    { params : Params
    , input : String
    , result : QueryResult
    , isLoading : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { params = Z3Params Z3.Model.default
      , input = ""
      , result = { histories = [], focused = 0 }
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
    = UpdateZ3Param Z3.Update.ParamMsg
    | UpdateCvc4Param CVC4.Update.ParamMsg


type Msg
    = ChangeSolver Solver
    | UpdateParam UpdateParamMsg
    | Input String
    | Verify
    | Output (Result Http.Error String)
    | SelectHistory Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeSolver solver ->
            ( { model
                | params =
                    case solver of
                        Z3Solver ->
                            Z3Params Z3.Model.default

                        Cvc4Solver ->
                            Cvc4Params CVC4.Model.default
              }
            , Cmd.none
            )

        UpdateParam paramMsg ->
            ( { model
                | params =
                    case ( model.params, paramMsg ) of
                        ( Z3Params params, UpdateZ3Param msg_ ) ->
                            Z3Params <| Z3.Update.update msg_ params

                        ( Cvc4Params params, UpdateCvc4Param msg_ ) ->
                            Cvc4Params <| CVC4.Update.update msg_ params

                        ( _, _ ) ->
                            undefined ()
              }
            , Cmd.none
            )

        Input src ->
            ( { model | input = src }, Cmd.none )

        Verify ->
            ( { model | isLoading = True }, verificationCommand model )

        Output output ->
            ( { model
                | isLoading = False
                , result =
                    let
                        new_history =
                            case output of
                                Ok content ->
                                    { isSuccess = True, content = content }

                                Err err ->
                                    { isSuccess = False, content = toString err }
                    in
                    { histories = new_history :: model.result.histories
                    , focused = 0
                    }
              }
            , Cmd.none
            )

        SelectHistory index ->
            let
                old_result =
                    model.result
            in
            ( { model | result = { old_result | focused = index } }, Cmd.none )



-- HTTP


verificationRequestBody : Model -> Http.Body
verificationRequestBody model =
    Http.jsonBody <|
        Json.Encode.object
            [ ( "src", Json.Encode.string model.input )
            , ( "argments"
              , case model.params of
                    Z3Params params ->
                        Z3.Model.jsonOfParams params

                    Cvc4Params params ->
                        CVC4.Model.jsonOfParams params
              )
            ]


verificationCommand : Model -> Cmd Msg
verificationCommand model =
    Http.post
        { url = "https://aa4fhzgok5.execute-api.us-east-2.amazonaws.com/product/verify"
        , body = verificationRequestBody model
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
            [ div [ class "column col-4-mr-auto" ] [ paramsView model.params ]
            , div [ class "column col-8" ] [ mainView model ]
            ]
        ]


selectSolverView : Params -> Html Msg
selectSolverView params =
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


paramsView : Params -> Html Msg
paramsView params =
    ul [ class "menu" ]
        [ li
            [ class "menu-item" ]
            [ selectSolverView params ]
        , li [ class "menu-item" ]
            [ case params of
                Z3Params params_ ->
                    Html.map (\msg -> UpdateParam <| UpdateZ3Param msg) <| Z3.View.view params_

                Cvc4Params params_ ->
                    Html.map (\msg -> UpdateParam <| UpdateCvc4Param msg) <| CVC4.View.view params_
            ]
        ]


mainView : Model -> Html Msg
mainView model =
    div [ class "form-group" ] <|
        [ label [ class "form-label", for "input-area" ] [ text "Query to Solver:" ]
        , textarea [ class "input-area form-input", rows 12, onInput Input ] []
        , button
            [ class <|
                if model.isLoading then
                    "btn loading"

                else
                    "btn"
            , onClick Verify
            ]
            [ text "verify!" ]
        , resultView model.result
        ]


resultView : QueryResult -> Html Msg
resultView result =
    case Util.nth result.histories result.focused of
        Nothing ->
            div [] []

        Just history ->
            div []
                [ ul [ class "tab tab-block" ] <|
                    List.indexedMap
                        (\index _ ->
                            li
                                [ class <|
                                    if result.focused == index then
                                        "tab-item active"

                                    else
                                        "tab-item"
                                , onClick (SelectHistory index)
                                ]
                                [ a
                                    [ href "#"
                                    , class <|
                                        if result.focused == index then
                                            "active"

                                        else
                                            ""
                                    ]
                                    [ text <| "#" ++ String.fromInt index ]
                                ]
                        )
                        result.histories
                , text history.content
                ]
