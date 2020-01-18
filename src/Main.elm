module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, button, div, img, text)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)
import Svg exposing (image, svg)
import Svg.Attributes exposing (height, viewBox, width, xlinkHref)



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
        [ svg [ width "128", height "128", viewBox "0 0 128 128" ]
            [ image [ xlinkHref "/icon128.png", width "128", height "128" ] []
            ]
        , div []
            [ img [ src "/icon128.png" ] []
            ]
        , text model
        , button [ onClick PostMessage ] [ text "Reply" ]
        ]
