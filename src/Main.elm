module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, a, button, div, img, text)
import Html.Attributes exposing (href, src)
import Html.Events exposing (onClick)
import Svg exposing (image, rect, svg)
import Svg.Attributes exposing (height, rx, ry, style, viewBox, width, x, xlinkHref, y)



-- MAIN


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }



-- MODEL


type alias Model =
    { x : Float, y : Float }


init : Model
init =
    { x = 28
    , y = 28
    }



---- UPDATE


type Msg
    = MoveRight


update : Msg -> Model -> Model
update msg model =
    { model | x = model.x + 32 }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ svg [ width "128", height "128", viewBox "0 0 128 128" ]
            [ image [ xlinkHref "/assets/images/icon128.png", width "128", height "128" ] []
            , rect [ x (String.fromFloat model.x), y "28", width "10", height "10", rx "20", ry "20" ] []
            ]
        , div []
            [ button [ onClick MoveRight ] [ text ">" ]
            ]
        , div []
            [ a [ href "https://github.com/ZimbiX/pathfinder-elm" ] [ text "GitHub" ]
            ]
        ]
