module Util exposing (createCheckboxLine, createMaybeIntInput, createSelectLine, jsonOfMaybeInt, onChange, toString, undefined)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as Events exposing (..)
import Http
import Json.Decode as Json
import Json.Encode


undefined : () -> a
undefined _ =
    Debug.todo "<undefined>"


toString : Http.Error -> String
toString err =
    case err of
        Http.Timeout ->
            "Timeout"

        Http.NetworkError ->
            "Network error"

        Http.BadStatus resp ->
            "BadStatus " ++ String.fromInt resp

        Http.BadUrl url ->
            "BadUrl: " ++ url

        Http.BadBody body ->
            "BadBody: " ++ body


onChange : (String -> msg) -> Attribute msg
onChange handler =
    on "change" (Json.map handler Events.targetValue)


createSelectLine : List String -> (String -> a) -> Html a
createSelectLine options msg =
    select [ class "form-select", onChange msg ]
        (List.map
            (\opt ->
                option [ value opt ] [ text opt ]
            )
            options
        )


createCheckboxLine : a -> String -> Html a
createCheckboxLine msg desc =
    label [ class "form-checkbox" ]
        [ input [ type_ "checkbox", onClick msg ] []
        , i [ class "form-icon" ] []
        , text desc
        ]


createMaybeIntInput : (Maybe Int -> a) -> String -> Maybe Int -> List (Html a)
createMaybeIntInput msg desc param =
    [ label [ class "form-checkbox" ]
        [ input
            [ type_ "checkbox"
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
    ]
        ++ (case param of
                Nothing ->
                    []

                Just _ ->
                    [ input
                        [ class "form-input"
                        , type_ "number"
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
           )


jsonOfMaybeInt : String -> Maybe Int -> List ( String, Json.Encode.Value )
jsonOfMaybeInt str x =
    case x of
        Nothing ->
            []

        Just n ->
            [ ( str, Json.Encode.int n ) ]
