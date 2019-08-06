module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, br, button, div, option, select, text, textarea)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode exposing (Decoder, field, string)
import Json.Encode


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- UTIL


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


stringfyOutput : Maybe String -> String
stringfyOutput x =
    case x of
        Just str ->
            str

        Nothing ->
            ""



-- MODEL


type alias Model =
    { solver : String
    , format : String
    , input : String
    , output : Maybe String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "" "" "" Nothing, Cmd.none )



-- UPDATE


type Msg
    = Solver String
    | Format String
    | Input String
    | Verify
    | GotResult (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Solver solver ->
            ( { model | solver = solver }, Cmd.none )

        Format format ->
            ( { model | format = format }, Cmd.none )

        Input src ->
            ( { model | input = src }, Cmd.none )

        Verify ->
            ( model, getVerificationResult model )

        GotResult result ->
            case result of
                Ok json ->
                    ( { model | output = Just <| Debug.log "success response: " json }, Cmd.none )

                Err err ->
                    ( { model | output = Just <| Debug.log "error response: " <| toString err }, Cmd.none )



-- HTTP


createVerificationRequestBody : Model -> Http.Body
createVerificationRequestBody model =
    Http.jsonBody <|
        Json.Encode.object
            [ ( "smt_source", Json.Encode.string model.input ) ]


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
    div []
        [ select [ onInput Solver ]
            [ option [ value "Z3" ] [ text "z3" ]
            ]
        , select [ onInput Format ]
            [ option [ value "Smtlib2" ] [ text "smtlib 2.6" ] ]
        , br [] []
        , textarea [ cols 40, rows 10, placeholder "...", onInput Input ] []
        , br [] []
        , button [ onClick Verify ] [ text "verify!" ]
        , br [] []
        , textarea [ cols 40, rows 10, placeholder "output" ] [ text <| stringfyOutput model.output ]
        ]
