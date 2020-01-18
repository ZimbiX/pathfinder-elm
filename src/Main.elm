module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, button, div, img, text)
import Html.Attributes exposing (href, src)
import Html.Events exposing (onClick)
import Svg exposing (image, svg)
import Svg.Attributes exposing (height, viewBox, width)



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
        [ --[ svg [ width "120", height "120", viewBox "0 0 120 120" ]
          --    [ image [ href "/icon128.png" ] []
          --    ]
          div []
            [ img [ src "/icon128.png" ] []
            ]
        , text model
        , button [ onClick PostMessage ] [ text "Reply" ]
        ]
