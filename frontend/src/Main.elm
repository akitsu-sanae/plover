module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import CVC4.Model
import CVC4.Update
import CVC4.View
import Html exposing (Html, a, button, code, div, h1, h4, h5, header, label, li, p, pre, text, textarea, ul)
import Html.Attributes exposing (class, for, href, id, rows)
import Html.Attributes.Aria exposing (ariaLabel, role)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode
import Json.Encode
import Util
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
    { statusCode : Int, stdout : String, stderr : String }


type alias QueryResult =
    { histories : List (Result Http.Error History), focused : Int }


isSuccess : Result Http.Error History -> Bool
isSuccess x =
    case x of
        Ok history ->
            history.statusCode == 0

        Err _ ->
            False


contentOfHistoryResult : Result Http.Error History -> Html Msg
contentOfHistoryResult x =
    let
        codeClass =
            class <|
                if isSuccess x then
                    "text-light bg-dark"

                else
                    "text-error bg-dark"
    in
    case x of
        Ok history ->
            div [ class "columns" ]
                [ div [ class "column col-12 text-light bg-dark" ] [ pre [] [ code [ codeClass ] [ text <| "status code: " ++ String.fromInt history.statusCode ] ] ]
                , div [ class "column col-12" ] [ text "stdout:" ]
                , div [ class "column col-12 text-light bg-dark" ] [ pre [] [ code [ codeClass ] [ text history.stdout ] ] ]
                , div [ class "column col-12" ] [ text "stderr:" ]
                , div [ class "column col-12 text-light bg-dark" ] [ pre [] [ code [ codeClass ] [ text history.stderr ] ] ]
                ]

        Err err ->
            div [ class "columns" ]
                [ div [ class "column col-12" ]
                    [ pre []
                        [ code [ codeClass ]
                            [ text <| "Network Error : " ++ Util.stringOfHttpError err ]
                        ]
                    ]
                ]


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
    | Output (Result Http.Error History)
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
                            Util.undefined ()
              }
            , Cmd.none
            )

        Input src ->
            ( { model | input = src }, Cmd.none )

        Verify ->
            ( { model | isLoading = True }, verifyCmd model )

        Output output ->
            ( { model
                | isLoading = False
                , result =
                    { histories = output :: model.result.histories
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


requestBody : Model -> Http.Body
requestBody model =
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


verifyCmd : Model -> Cmd Msg
verifyCmd model =
    Http.post
        { url = "https://aa4fhzgok5.execute-api.us-east-2.amazonaws.com/product/verify"
        , body = requestBody model
        , expect = Http.expectJson Output resultDecoder
        }


resultDecoder : Json.Decode.Decoder History
resultDecoder =
    Json.Decode.map3 History
        (Json.Decode.field "exit" Json.Decode.int)
        (Json.Decode.field "stdout" Json.Decode.string)
        (Json.Decode.field "stderr" Json.Decode.string)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ headerView
        , div [ class "columns" ]
            [ div [ class "column col-4-mr-auto" ] [ paramsView model.params ]
            , div [ class "column col-8" ] [ mainView model ]
            ]
        ]


copyrightView : Html Msg
copyrightView =
    div [ class "content" ]
        [ h4 [] [ text "Plover Garden" ]
        , p []
            [ text
                "Copyright (C) 2019 akitsu sanae.\n                        Distributed under the Boost Software License, Version 1.0. (See accompanying file LICENSE_1_0.txt)"
            ]
        , p [] [ text "Plover Garden depends on ..." ]
        , h5 [] [ text "Z3" ]
        , p [] [ text "Copyright (c) Microsoft Corporation" ]
        , p [] [ text "All rights reserved." ]
        , p [] [ text "MIT License" ]
        , p [] [ text "Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the \" Software \"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:" ]
        , p [] [ text "The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software." ]
        , p [] [ text "THE SOFTWARE IS PROVIDED *AS IS*, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE." ]
        , h5 [] [ text "CVC4" ]
        , p [] [ text """CVC4 is copyright (C) 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017 by
            its authors and contributors (see the file https://github.com/CVC4/CVC4/blob/master/AUTHORS) and their institutional
            affiliations.  All rights reserved.""" ]
        , p [] [ text """The source code of CVC4 is open and available to students, researchers,
            software companies, and everyone else to study, to modify, and to redistribute
            original or modified versions; distribution is under the terms of the modified
            BSD license (reproduced below).  Please note that CVC4 can be configured
            (however, by default it is not) to link against some GPLed libraries, and
            therefore the use of these builds may be restricted in non-GPL-compatible
            projects.  See below for a discussion of CLN, GLPK, and Readline (the three
            GPLed optional library dependences for CVC4), and how to ensure you have a
            build that doesn't link against GPLed libraries.""" ]
        , p [] [ text """Redistribution and use in source and binary forms, with or without
            modification, are permitted provided that the following conditions are
            met:""" ]
        , p [] [ text """1. Redistributions of source code must retain the above copyright
            notice, this list of conditions and the following disclaimer.""" ]
        , p [] [ text """2. Redistributions in binary form must reproduce the above copyright
            notice, this list of conditions and the following disclaimer in the
            documentation and/or other materials provided with the distribution.""" ]
        , p [] [ text """3. Neither the name of the copyright holder nor the names of its
            contributors may be used to endorse or promote products derived from
            this software without specific prior written permission.""" ]
        , p [] [ text """THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT OWNERS AND CONTRIBUTORS
            ''AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
            LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
            A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
            OWNERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
            SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
            LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
            DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
            THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
            (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
            OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.""" ]
        , h5 [] [ text "Spectre" ]
        , p [] [ text "The MIT License (MIT)" ]
        , p [] [ text "Copyright (c) 2016 - 2018 Yan Zhu" ]
        , p [] [ text """Permission is hereby granted, free of charge, to any person obtaining a copy
            of this software and associated documentation files (the "Software"), to deal
            in the Software without restriction, including without limitation the rights
            to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
            copies of the Software, and to permit persons to whom the Software is
            furnished to do so, subject to the following conditions:""" ]
        , p [] [ text """The above copyright notice and this permission notice shall be included in all
            copies or substantial portions of the Software.""" ]
        , p [] [ text """THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
            IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
            FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
            AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
            LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
            OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
            SOFTWARE.""" ]
        ]


copyrightNoticeView : List (Html Msg)
copyrightNoticeView =
    [ a [ class "btn btn-primary", href "#copyright-modal" ] [ text "copyright" ]
    , div [ class "modal", id "copyright-modal" ]
        [ a [ class "modal-overlay", href "#modals", ariaLabel "Close" ] []
        , div [ class "modal-container", role "document" ]
            [ div [ class "modal-header" ]
                [ div [ class "modal-title h5" ] [ text "Copyright" ] ]
            , div [ class "modal-body" ] [ copyrightView ]
            , div [ class "modal-footer" ] []
            ]
        ]
    ]


headerView : Html Msg
headerView =
    header []
        [ div [ class "columns" ]
            [ div [ class "column col-auto" ] [ h1 [] [ text "Plover Garden" ] ]
            , div [ class "column col-2 col-ml-auto" ] copyrightNoticeView
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
                , contentOfHistoryResult history
                ]
