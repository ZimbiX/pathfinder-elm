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
    { column : Float
    , row : Float
    , walls : List Wall
    }


type alias Wall =
    { column : Float
    , row : Float
    , orientation : Orientation
    }


type Orientation
    = Horizontal
    | Vertical


gridBorder =
    { top = 0
    , left = 0
    }


gridSize =
    { rowCount = 15
    , columnCount = 15
    , cellWidth = 32
    , cellHeight = 32
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { column = 0
      , row = 0
      , walls =
            [ { column = 0, row = 0, orientation = Horizontal }
            , { column = 1, row = 0, orientation = Horizontal }
            , { column = 2, row = 0, orientation = Horizontal }
            , { column = 3, row = 0, orientation = Horizontal }
            , { column = 4, row = 0, orientation = Horizontal }
            , { column = 0, row = 5, orientation = Horizontal }
            , { column = 1, row = 5, orientation = Horizontal }
            , { column = 2, row = 5, orientation = Horizontal }
            , { column = 3, row = 5, orientation = Horizontal }
            , { column = 4, row = 5, orientation = Horizontal }
            , { column = 0, row = 0, orientation = Vertical }
            , { column = 0, row = 1, orientation = Vertical }
            , { column = 0, row = 2, orientation = Vertical }
            , { column = 0, row = 3, orientation = Vertical }
            , { column = 0, row = 4, orientation = Vertical }
            , { column = 5, row = 0, orientation = Vertical }
            , { column = 5, row = 1, orientation = Vertical }
            , { column = 5, row = 2, orientation = Vertical }
            , { column = 5, row = 3, orientation = Vertical }
            , { column = 5, row = 4, orientation = Vertical }
            ]
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
        [ svg [ width "640", height "480", viewBox "0 0 640 480" ]
            (List.concat
                [ [ image [ xlinkHref "/assets/images/bg_level.png", width "640", height "480" ] []
                  , viewPlayer model
                  ]
                , model.walls |> List.map viewWall
                ]
            )
        , viewArrowButtons
        , viewGithubLink
        ]


xFromColumn : Float -> Float
xFromColumn column =
    gridBorder.left + gridSize.cellWidth * (0.5 + column)


yFromRow : Float -> Float
yFromRow row =
    gridBorder.top + gridSize.cellHeight * (0.5 + row)


viewPlayer model =
    image
        [ xlinkHref "/assets/images/man.png"
        , width "11"
        , height "31"
        , x ((model.column |> xFromColumn) - (11 / 2) |> round |> String.fromInt)
        , y ((model.row |> yFromRow) - 31 / 2 + 1 |> round |> String.fromInt)
        ]
        []


viewWall wall =
    if wall.orientation == Vertical then
        image
            [ xlinkHref "/assets/images/wall_v.png"
            , width "16"
            , height "32"
            , x ((wall.column |> xFromColumn) - 24 |> round |> String.fromInt)
            , y ((wall.row |> yFromRow) - 15 |> round |> String.fromInt)
            ]
            []

    else
        image
            [ xlinkHref "/assets/images/wall_h.png"
            , width "32"
            , height "16"
            , x ((wall.column |> xFromColumn) - 16 |> round |> String.fromInt)
            , y ((wall.row |> yFromRow) - 24 |> round |> String.fromInt)
            ]
            []


viewArrowButtons =
    div []
        [ div []
            [ button [ onClick (Move MoveRight) ] [ text ">" ]
            , button [ onClick (Move MoveLeft) ] [ text "<" ]
            , button [ onClick (Move MoveUp) ] [ text "^" ]
            , button [ onClick (Move MoveDown) ] [ text "v" ]
            ]
        , div [] [ text "or use arrow keys" ]
        ]


viewGithubLink =
    div [] [ a [ href "https://github.com/ZimbiX/pathfinder-elm" ] [ text "GitHub" ] ]
