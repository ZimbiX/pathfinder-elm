module Main exposing (Model, Msg(..), init, main, update, view)

--import Browser.Document

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Css exposing (absolute, fontSize, height, left, position, px, top, width)
import Debug
import Html exposing (Html)
import Html.Styled exposing (a, button, div, img, text, toUnstyled)
import Html.Styled.Attributes exposing (css, href, src)
import Html.Styled.Events exposing (onClick)
import Keyboard exposing (Key(..), RawKey)
import Keyboard.Arrows
import List.Extra



-- MAIN


main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions

        --, subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { column : Float
    , row : Float
    , currentMove : CurrentMove
    , clock : Float
    , walls : Walls
    }


type alias Walls =
    List Wall


type alias CurrentMove =
    Maybe
        { origin : Position
        , target : Position
        , direction : MoveDirection
        , reversing : Bool
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
            [ { column = 0, row = -0.5, orientation = Horizontal }
            , { column = 1, row = -0.5, orientation = Horizontal }
            , { column = 2, row = -0.5, orientation = Horizontal }
            , { column = 3, row = -0.5, orientation = Horizontal }
            , { column = 4, row = -0.5, orientation = Horizontal }
            , { column = 0, row = 4.5, orientation = Horizontal }
            , { column = 1, row = 4.5, orientation = Horizontal }
            , { column = 2, row = 4.5, orientation = Horizontal }
            , { column = 3, row = 4.5, orientation = Horizontal }
            , { column = 4, row = 4.5, orientation = Horizontal }
            , { column = -0.5, row = 0, orientation = Vertical }
            , { column = -0.5, row = 1, orientation = Vertical }
            , { column = -0.5, row = 2, orientation = Vertical }
            , { column = -0.5, row = 3, orientation = Vertical }
            , { column = -0.5, row = 4, orientation = Vertical }
            , { column = 4.5, row = 0, orientation = Vertical }
            , { column = 4.5, row = 1, orientation = Vertical }
            , { column = 4.5, row = 2, orientation = Vertical }
            , { column = 4.5, row = 3, orientation = Vertical }
            , { column = 4.5, row = 4, orientation = Vertical }
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
            ( updatePlayerPosition deltaTime model
                |> returnToOriginIfPathUnclear
                |> finishPlayerMove
            , Cmd.none
            )


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

            else if currentMove.reversing then
                { model | column = currentMove.origin.column, row = currentMove.origin.row, currentMove = Nothing }

            else
                { model | column = currentMove.target.column, row = currentMove.target.row, currentMove = Nothing }

        Nothing ->
            model


returnToOriginIfPathUnclear : Model -> Model
returnToOriginIfPathUnclear model =
    if pathAheadClear model then
        model

    else
        case model.currentMove of
            Just currentMove ->
                if currentMove.reversing then
                    model

                else
                    { model | currentMove = Just { currentMove | reversing = True, direction = oppositeDirection currentMove.direction } }

            -- TODO: Compensate for overshooting the point of collision
            Nothing ->
                model


oppositeDirection : MoveDirection -> MoveDirection
oppositeDirection direction =
    case direction of
        MoveLeft ->
            MoveRight

        MoveRight ->
            MoveLeft

        MoveUp ->
            MoveDown

        MoveDown ->
            MoveUp

        NoMove ->
            NoMove


pathAheadClear : Model -> Bool
pathAheadClear model =
    case model.currentMove of
        Just currentMove ->
            if wallExistsBetweenPoints model.walls currentMove.origin currentMove.target then
                (numberBetween currentMove.origin.column (currentMove.origin.column + 0.4) model.column
                    || numberBetween currentMove.origin.column (currentMove.origin.column - 0.4) model.column
                )
                    && (numberBetween currentMove.origin.row (currentMove.origin.row + 0.4) model.row
                            || numberBetween currentMove.origin.row (currentMove.origin.row - 0.4) model.row
                       )

            else
                True

        Nothing ->
            True


wallExistsBetweenPoints : Walls -> Position -> Position -> Bool
wallExistsBetweenPoints walls pointA pointB =
    let
        midpoint =
            { column = (pointA.column + pointB.column) / 2
            , row = (pointA.row + pointB.row) / 2
            }

        wall =
            walls |> List.Extra.find (wallIsAtPoint midpoint)
    in
    case wall of
        Just _ ->
            True

        Nothing ->
            False


wallIsAtPoint : Position -> Wall -> Bool
wallIsAtPoint point wall =
    (wall.column == point.column)
        && (wall.row == point.row)


playerWithinMoveBounds : Model -> { direction : MoveDirection, origin : Position, target : Position, reversing : Bool } -> Bool
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
            { model | currentMove = Just { direction = moveDirection, origin = origin, target = { origin | column = origin.column + 1 }, reversing = False } }

        MoveLeft ->
            { model | currentMove = Just { direction = moveDirection, origin = origin, target = { origin | column = origin.column - 1 }, reversing = False } }

        MoveUp ->
            { model | currentMove = Just { direction = moveDirection, origin = origin, target = { origin | row = origin.row - 1 }, reversing = False } }

        MoveDown ->
            { model | currentMove = Just { direction = moveDirection, origin = origin, target = { origin | row = origin.row + 1 }, reversing = False } }

        NoMove ->
            model



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.downs KeyDown
        , onAnimationFrameDelta Tick
        ]



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "PathFinder"
    , body =
        [ div []
            [ viewBoard model
            , viewArrowButtons
            , viewGithubLink
            ]
            |> toUnstyled
        ]
    }


xFromColumn : Float -> Float
xFromColumn column =
    gridBorder.left + gridSize.cellWidth * (0.5 + column)


yFromRow : Float -> Float
yFromRow row =
    gridBorder.top + gridSize.cellHeight * (0.5 + row)


roundFloat =
    round >> toFloat


viewBoard model =
    div [ css [ width (px 640), height (px 480) ] ]
        (List.concat
            [ [ viewBackground
              , viewPlayer model
              ]
            , viewWalls model
            ]
        )


viewBackground =
    img [ src "/assets/images/bg_level.png", css [ width (px 640), height (px 480) ] ] []


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


viewWalls model =
    model.walls |> List.map viewWall


viewWall wall =
    if wall.orientation == Vertical then
        img
            [ src "/assets/images/wall_v.png"
            , css
                [ position absolute
                , width (px 8)
                , height (px 32)
                , left (px ((wall.column |> xFromColumn) - 4 |> roundFloat))
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
                , top (px ((wall.row |> yFromRow) - 4 |> roundFloat))
                ]
            ]
            []


viewArrowButtons =
    div []
        [ div []
            [ viewArrowButton MoveLeft "<"
            , viewArrowButton MoveRight ">"
            , viewArrowButton MoveUp "^"
            , viewArrowButton MoveDown "v"
            ]
        , div [] [ text "or use arrow keys" ]
        ]


viewArrowButton moveDirection buttonText =
    button
        [ css
            [ width (px 50)
            , height (px 50)
            , fontSize (px 30)
            ]
        , onClick (Move moveDirection)
        ]
        [ text buttonText ]


viewGithubLink =
    div [] [ a [ href "https://github.com/ZimbiX/pathfinder-elm" ] [ text "GitHub" ] ]
