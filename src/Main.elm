module Main exposing (Model, Msg(..), init, main, update, view)

import Animation exposing (Animation, animation, duration, from, to)
import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Css exposing (absolute, fontSize, height, left, position, px, top, width)
import Debug
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

        --, subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { column : Float
    , row : Float
    , currentMove : CurrentMove
    , clock : Float
    , walls : List Wall
    }


type alias CurrentMove =
    Maybe
        { origin : Position
        , target : Position
        , direction : MoveDirection
        }


type alias Position =
    { column : Float, row : Float }


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
      , currentMove = Nothing
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
        Move moveDirection ->
            ( tryStartPlayerMove moveDirection model, Cmd.none )

        KeyDown rawKey ->
            ( tryStartPlayerMove (moveDirectionFromKeyDown rawKey) model, Cmd.none )

        Tick deltaTime ->
            ( updatePlayerPosition deltaTime model |> finishPlayerMove, Cmd.none )


updatePlayerPosition : Float -> Model -> Model
updatePlayerPosition deltaTime model =
    let
        clock =
            model.clock + deltaTime
    in
    case model.currentMove of
        Just currentMove ->
            let
                moveSpeed =
                    0.007

                moveDistance =
                    moveSpeed * deltaTime

                column =
                    case currentMove.direction of
                        MoveRight ->
                            model.column + moveDistance

                        MoveLeft ->
                            model.column - moveDistance

                        _ ->
                            model.column

                row =
                    case currentMove.direction of
                        MoveDown ->
                            model.row + moveDistance

                        MoveUp ->
                            model.row - moveDistance

                        _ ->
                            model.row
            in
            { model | clock = clock, column = column, row = row }

        Nothing ->
            { model | clock = clock }


tryStartPlayerMove : MoveDirection -> Model -> Model
tryStartPlayerMove moveDirection model =
    case model.currentMove of
        Just _ ->
            model

        Nothing ->
            startPlayerMove moveDirection model


finishPlayerMove : Model -> Model
finishPlayerMove model =
    case model.currentMove of
        Just currentMove ->
            if playerWithinMoveBounds model currentMove then
                model

            else
                { model | column = currentMove.target.column, row = currentMove.target.row, currentMove = Nothing }

        Nothing ->
            model


playerWithinMoveBounds : Model -> { direction : MoveDirection, origin : Position, target : Position } -> Bool
playerWithinMoveBounds model currentMove =
    let
        columnWithinMoveBounds =
            numberBetween currentMove.origin.column currentMove.target.column model.column

        rowWithinMoveBounds =
            numberBetween currentMove.origin.row currentMove.target.row model.row
    in
    columnWithinMoveBounds && rowWithinMoveBounds


numberBetween : Float -> Float -> Float -> Bool
numberBetween a b num =
    a
        <= num
        && num
        <= b
        || b
        <= num
        && num
        <= a


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


startPlayerMove : MoveDirection -> Model -> Model
startPlayerMove moveDirection model =
    let
        origin =
            { column = model.column, row = model.row }
    in
    case moveDirection of
        MoveRight ->
            { model | currentMove = Just { direction = moveDirection, origin = origin, target = { origin | column = origin.column + 1 } } }

        MoveLeft ->
            { model | currentMove = Just { direction = moveDirection, origin = origin, target = { origin | column = origin.column - 1 } } }

        MoveUp ->
            { model | currentMove = Just { direction = moveDirection, origin = origin, target = { origin | row = origin.row - 1 } } }

        MoveDown ->
            { model | currentMove = Just { direction = moveDirection, origin = origin, target = { origin | row = origin.row + 1 } } }

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
        , viewArrowButtons model
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


viewArrowButtons model =
    div []
        [ div []
            [ button [ css [ width (px 50), height (px 50), fontSize (px 30) ], onClick (Move MoveLeft) ] [ text "<" ]
            , button [ css [ width (px 50), height (px 50), fontSize (px 30) ], onClick (Move MoveRight) ] [ text ">" ]
            , button [ css [ width (px 50), height (px 50), fontSize (px 30) ], onClick (Move MoveUp) ] [ text "^" ]
            , button [ css [ width (px 50), height (px 50), fontSize (px 30) ], onClick (Move MoveDown) ] [ text "v" ]
            ]
        , div [] [ text "or use arrow keys" ]
        ]


enableOrDisableTextFromBool flag =
    if flag then
        "Disable"

    else
        "Enable"


viewGithubLink =
    div [] [ a [ href "https://github.com/ZimbiX/pathfinder-elm" ] [ text "GitHub" ] ]
