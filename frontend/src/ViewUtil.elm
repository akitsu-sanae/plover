module ViewUtil exposing (checkboxColumn, checkboxNumberColumn, inputNumberColumn, onChange, registerParamsColumn, selectColumn)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as Events exposing (..)
import Json.Decode
import Util exposing (undefined)


onChange : (String -> msg) -> Attribute msg
onChange handler =
    on "change" (Json.Decode.map handler Events.targetValue)


selectColumn : List String -> (String -> a) -> String -> Html a
selectColumn options msg desc =
    div [ class "column col-12" ]
        [ text desc
        , select [ class "form-select", onChange msg ]
            (List.map
                (\opt ->
                    option [ value opt ] [ text opt ]
                )
                options
            )
        ]


inputNumberColumn : (Int -> a) -> String -> Html a
inputNumberColumn msg desc =
    div [ class "column col-12" ]
        [ text desc
        , input
            [ class "form-input input-sm"
            , type_ "number"
            , Html.Attributes.min "0"
            , onInput
                (\str ->
                    case String.toInt str of
                        Nothing ->
                            undefined ()

                        Just n ->
                            msg n
                )
            ]
            []
        ]


checkboxColumn : a -> String -> Html a
checkboxColumn msg desc =
    div [ class "column col-12" ]
        [ label [ class "form-checkbox label-sm" ]
            [ input [ type_ "checkbox", class "input-sm", onClick msg ] []
            , i [ class "form-icon" ] []
            , text desc
            ]
        ]


checkboxNumberColumn : (Maybe Int -> a) -> String -> Maybe Int -> Html a
checkboxNumberColumn msg desc param =
    div [ class "column col-12 input-group" ]
        [ label [ class "form-checkbox label-sm" ]
            [ input
                [ type_ "checkbox"
                , class "input-sm"
                , checked (param /= Nothing)
                , onInput
                    (\x ->
                        case param of
                            Nothing ->
                                msg (Just 0)

                            Just n ->
                                msg Nothing
                    )
                ]
                []
            , i [ class "form-icon" ] []
            , text <| desc ++ ":"
            ]
        , input
            [ class "form-input input-sm"
            , type_ "number"
            , disabled (param == Nothing)
            , Html.Attributes.min "0"
            , onInput
                (\str ->
                    case String.toInt str of
                        Nothing ->
                            undefined ()

                        Just n ->
                            msg (Just n)
                )
            ]
            []
        ]


registerParamsColumn : (String -> msg) -> (Int -> msg) -> (String -> msg) -> String -> { options : List String, currentInput : String } -> Html msg
registerParamsColumn addHandler removeHandler inputtingHandler label formData =
    let
        registerInput =
            [ div [ class "column col-12 input-group" ]
                [ div [ class "columns" ]
                    [ input [ class "form-input input-sm", type_ "text", onInput inputtingHandler ] []
                    , button [ class "btn btn-sm btn-primary", onClick (addHandler formData.currentInput) ] [ text "add" ]
                    ]
                ]
            ]

        ( options, _ ) =
            List.foldl
                (\str ( list_, n ) ->
                    ( div
                        [ class "column col-12" ]
                        [ div
                            [ class "columns" ]
                            [ div [ class "col-mr-auto" ] [ span [ class "label label-sm" ] [ text str ] ]
                            , div [ class "col-2" ] [ button [ class "btn btn-sm", onClick (removeHandler n) ] [ text "×" ] ]
                            ]
                        ]
                        :: list_
                    , n + 1
                    )
                )
                ( registerInput, 0 )
                formData.options
    in
    div [ class "column col-12 input-group" ]
        [ div [ class "columns" ] <| text label :: options ]
