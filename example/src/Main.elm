module Main exposing (main)

import Browser
import Crockford
import Html exposing (Html, div, input, text)
import Html.Events exposing (onInput)


main =
    Browser.sandbox { init = init, update = update, view = view }


type Msg
    = SetNumber String


type alias Model =
    { num : Int
    }


init : Model
init =
    { num = 0
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetNumber str ->
            { model | num = Maybe.withDefault 0 (String.toInt str) }


view : Model -> Html Msg
view model =
    div []
        [ input [ onInput SetNumber ] []
        , div [] [ text "base32: ", text (Result.withDefault "" (Crockford.encode model.num)) ]
        , div [] [ text "base32 with checksum: ", text (Result.withDefault "" (Crockford.encodeWithChecksum model.num)) ]
        ]
