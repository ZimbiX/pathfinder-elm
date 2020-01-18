module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, a, button, div, img, text)
import Html.Attributes exposing (href, src)
import Html.Events exposing (onClick)
import Keyboard exposing (Key(..), RawKey)
import Keyboard.Arrows
import Svg exposing (image, rect, svg)
import Svg.Attributes exposing (height, rx, ry, style, viewBox, width, x, xlinkHref, y)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { column : Float, row : Float }


gridBorder =
    { top = 16
    , left = 16
    }


gridSize =
    { rowCount = 3
    , columnCount = 3
    , cellWidth = 32
    , cellHeight = 32
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { column = 0
      , row = 0
      }
    , Cmd.none
    )



---- UPDATE


type Msg
    = Move MoveDirection
    | KeyDown RawKey


type MoveDirection
    = MoveRight
    | MoveLeft
    | MoveUp
    | MoveDown
    | NoMove


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        moveDirection =
            case msg of
                Move md ->
                    md

                KeyDown rawKey ->
                    moveDirectionFromKeyDown rawKey
    in
    ( movePlayer moveDirection model |> validMove model
    , Cmd.none
    )


moveDirectionFromKeyDown : RawKey -> MoveDirection
moveDirectionFromKeyDown rawKey =
    case Keyboard.anyKeyUpper rawKey of
        Just ArrowRight ->
            MoveRight

        Just ArrowLeft ->
            MoveLeft

        Just ArrowUp ->
            MoveUp

        Just ArrowDown ->
            MoveDown

        Just (Character "D") ->
            MoveRight

        Just (Character "A") ->
            MoveLeft

        Just (Character "W") ->
            MoveUp

        Just (Character "S") ->
            MoveDown

        _ ->
            NoMove


movePlayer : MoveDirection -> Model -> Model
movePlayer moveDirection model =
    case moveDirection of
        MoveRight ->
            { model | column = model.column + 1 }

        MoveLeft ->
            { model | column = model.column - 1 }

        MoveUp ->
            { model | row = model.row - 1 }

        MoveDown ->
            { model | row = model.row + 1 }

        NoMove ->
            model


validMove : Model -> Model -> Model
validMove modelOrig modelNew =
    if
        modelNew.row
            >= 0
            && modelNew.column
            >= 0
            && modelNew.row
            < gridSize.rowCount
            && modelNew.column
            < gridSize.columnCount
    then
        modelNew

    else
        modelOrig



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.downs KeyDown
        ]



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
                , x ((model.column |> xFromColumn) - (11 / 2) |> round |> String.fromInt)
                , y ((model.row |> yFromRow) - 31 / 2 + 1 |> round |> String.fromInt)
                ]
                []
            ]
        , div []
            [ button [ onClick (Move MoveRight) ] [ text ">" ]
            , button [ onClick (Move MoveLeft) ] [ text "<" ]
            , button [ onClick (Move MoveUp) ] [ text "^" ]
            , button [ onClick (Move MoveDown) ] [ text "v" ]
            ]
        , div []
            [ a [ href "https://github.com/ZimbiX/pathfinder-elm" ] [ text "GitHub" ]
            ]
        ]


xFromColumn : Float -> Float
xFromColumn column =
    gridBorder.left + gridSize.cellWidth * (0.5 + column)


yFromRow : Float -> Float
yFromRow row =
    gridBorder.top + gridSize.cellHeight * (0.5 + row)
