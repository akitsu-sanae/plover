module Util exposing (createCheckboxLine, createSelectLine, onChange, toString, undefined)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as Events exposing (..)
import Http
import Json.Decode as Json


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
