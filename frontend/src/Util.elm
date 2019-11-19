module Util exposing (id, nth, remove, stringOfHttpError, undefined, unwrap, zip)

import Http


undefined : () -> a
undefined _ =
    Debug.todo "<undefined>"


stringOfHttpError : Http.Error -> String
stringOfHttpError err =
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


id : a -> a
id x =
    x


zip : List a -> List b -> List ( a, b )
zip l r =
    case ( l, r ) of
        ( x :: xs, y :: ys ) ->
            ( x, y ) :: zip xs ys

        _ ->
            []


unwrap : Maybe a -> a
unwrap x =
    case x of
        Nothing ->
            undefined ()

        Just y ->
            y


nth : List a -> Int -> Maybe a
nth l n =
    case ( l, n ) of
        ( [], _ ) ->
            Nothing

        ( head :: _, 0 ) ->
            Just head

        ( _ :: tail, _ ) ->
            nth tail (n - 1)


remove : List a -> Int -> List a
remove lst n =
    case ( lst, n ) of
        ( x :: xs, 0 ) ->
            xs

        ( x :: xs, m ) ->
            x :: remove xs (m - 1)

        ( [], _ ) ->
            undefined ()
