module ViewUtil exposing (checkboxColumn, checkboxNumberColumn, inputNumberColumn, onChange, selectColumn)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as Events exposing (..)
import Json.Decode as Json
import Util exposing (undefined)


onChange : (String -> msg) -> Attribute msg
onChange handler =
    on "change" (Json.map handler Events.targetValue)


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
