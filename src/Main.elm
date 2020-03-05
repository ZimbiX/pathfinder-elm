module Main exposing (Model, Msg(..), init, main, update, view)

import Animation exposing (Animation, animation, duration, from, to)
import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Css exposing (absolute, height, left, position, px, top, width)
import Html.Styled exposing (Html, a, button, div, img, text, toUnstyled)
import Html.Styled.Attributes exposing (css, href, src)
import Html.Styled.Events exposing (onClick)
import Keyboard exposing (Key(..), RawKey)
import Keyboard.Arrows



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , view = view >> toUnstyled
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { column : Float
    , row : Float
    , move : MoveDirection
    , clock : Float
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
      , move = NoMove
      , clock = 0
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
    = Tick Float
    | Move MoveDirection
    | KeyDown RawKey


type MoveDirection
    = MoveRight
    | MoveLeft
    | MoveUp
    | MoveDown
    | NoMove


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyDown rawKey ->
            ( { model | move = moveDirectionFromKeyDown rawKey }, Cmd.none )

        Tick deltaTime ->
            let
                clock =
                    model.clock + deltaTime

                column =
                    model.column + 0.001 * deltaTime
            in
            ( { model | clock = clock, column = column }, Cmd.none )

        _ ->
            ( model, Cmd.none )



--let
--    moveDirection =
--        case msg of
--            Move md ->
--                md
--            KeyDown rawKey ->
--                moveDirectionFromKeyDown rawKey
--in
--( movePlayer moveDirection model |> validMove model
--, Cmd.none
--)


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
        , onAnimationFrameDelta Tick
        ]



-- ANIMATION


myAnim : Animation
myAnim =
    animation 0
        |> from 0
        |> to 1
        |> duration 4



--animate
-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div
            [ css
                [ width (px 640)
                , height (px 480)
                ]
            ]
            (List.concat
                [ [ img [ src "/assets/images/bg_level.png", css [ width (px 640), height (px 480) ] ] []
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


roundFloat =
    round >> toFloat


viewPlayer model =
    img
        [ src "/assets/images/man.png"
        , css
            [ position absolute
            , width (px 11)
            , height (px 31)
            , left (px ((model.column |> xFromColumn) - (11 / 2) |> roundFloat))
            , top (px ((model.row |> yFromRow) - 31 / 2 + 1 |> roundFloat))
            ]
        ]
        []


viewWall wall =
    if wall.orientation == Vertical then
        img
            [ src "/assets/images/wall_v.png"
            , css
                [ position absolute
                , width (px 8)
                , height (px 32)
                , left (px ((wall.column |> xFromColumn) - 20 |> roundFloat))
                , top (px ((wall.row |> yFromRow) - 15 |> roundFloat))
                ]
            ]
            []

    else
        img
            [ src "/assets/images/wall_h.png"
            , css
                [ position absolute
                , width (px 32)
                , height (px 8)
                , left (px ((wall.column |> xFromColumn) - 16 |> roundFloat))
                , top (px ((wall.row |> yFromRow) - 20 |> roundFloat))
                ]
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
