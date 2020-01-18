module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)



-- MAIN


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }



-- MODEL


type alias Model =
    String


init : Model
init =
    "yo"



---- UPDATE


type Msg
    = PostMessage


update : Msg -> Model -> Model
update msg model =
    model ++ " sup"



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ text model
        , button [ onClick PostMessage ] [ text "Reply" ]
        ]
