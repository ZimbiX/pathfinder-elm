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


gridBorder =
    { top = 16
    , left = 16
    }


gridSize =
    { cellWidth = 32
    , cellHeight = 32
    }


init : Model
init =
    { x = gridBorder.left + gridSize.cellWidth / 2
    , y = gridBorder.top + gridSize.cellHeight / 2
    }



---- UPDATE


type Msg
    = MoveRight
    | MoveLeft
    | MoveUp
    | MoveDown


update : Msg -> Model -> Model
update msg model =
    case msg of
        MoveRight ->
            { model | x = model.x + gridSize.cellWidth }

        MoveLeft ->
            { model | x = model.x - gridSize.cellWidth }

        MoveUp ->
            { model | y = model.y - gridSize.cellHeight }

        MoveDown ->
            { model | y = model.y + gridSize.cellHeight }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ svg [ width "128", height "128", viewBox "0 0 128 128" ]
            [ image [ xlinkHref "/assets/images/icon128.png", width "128", height "128" ] []
            , image
                [ xlinkHref "/assets/images/man.png"
                , width "11"
                , height "31"
                , x (model.x - (11 / 2) |> round |> String.fromInt)
                , y (model.y - (31 / 2) + 1 |> round |> String.fromInt)
                ]
                []
            ]
        , div []
            [ button [ onClick MoveRight ] [ text ">" ]
            , button [ onClick MoveLeft ] [ text "<" ]
            , button [ onClick MoveUp ] [ text "^" ]
            , button [ onClick MoveDown ] [ text "v" ]
            ]
        , div []
            [ a [ href "https://github.com/ZimbiX/pathfinder-elm" ] [ text "GitHub" ]
            ]
        ]
