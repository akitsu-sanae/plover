module UiUtil exposing (createCheckboxLine, createMaybeIntInput, createSelectLine, onChange)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as Events exposing (..)
import Json.Decode as Json
import Util exposing (undefined)


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
    label [ class "form-checkbox label-sm" ]
        [ input [ type_ "checkbox", class "input-sm", onClick msg ] []
        , i [ class "form-icon" ] []
        , text desc
        ]


createMaybeIntInput : (Maybe Int -> a) -> String -> Maybe Int -> List (Html a)
createMaybeIntInput msg desc param =
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
    ]
        ++ (case param of
                Nothing ->
                    []

                Just _ ->
                    [ input
                        [ class "form-input input-sm"
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
