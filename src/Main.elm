module Main exposing (Model, Msg(..), init, main, update, view)

import Array
import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Css exposing (absolute, backgroundColor, border3, fontSize, height, hex, left, opacity, position, px, rad, rotate, solid, top, transform, width)
import Debug
import Html exposing (Html)
import Html.Events.Extra
import Html.Styled exposing (a, button, div, img, text, toUnstyled)
import Html.Styled.Attributes exposing (css, draggable, href, src)
import Html.Styled.Events exposing (on, onClick)
import Json.Decode
import Keyboard exposing (Key(..), RawKey)
import Keyboard.Arrows
import List.Extra
import Maybe.Extra



-- SETTINGS


settings =
    { playerMoveSpeed = 0.007
    , wallOpacitySpeed = 0.008
    }



-- MAIN


main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { position : Position
    , currentMove : CurrentMove
    , clock : Float
    , walls : Walls
    , mouse : Mouse
    , drawing : Drawing
    , snappedDrawingPoints : SnappedDrawingPoints
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
    , hidden : Bool
    , opacity : Float
    }


type Orientation
    = Horizontal
    | Vertical


type alias Mouse =
    { position : Coordinate
    , buttonDown : MouseButton
    }


type MouseButton
    = LeftMouseButton
    | RightMouseButton
    | NoMouseButton


type alias Drawing =
    List Coordinate


type alias SnappedDrawingPoints =
    List Position


type alias Coordinate =
    { x : Float, y : Float }


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
    ( { position =
            { column = 0
            , row = 0
            }
      , currentMove = Nothing
      , clock = 0
      , walls =
            [ { hidden = True, opacity = 0, column = 0, row = -0.5, orientation = Horizontal }
            , { hidden = True, opacity = 0, column = 1, row = -0.5, orientation = Horizontal }
            , { hidden = True, opacity = 0, column = 2, row = -0.5, orientation = Horizontal }
            , { hidden = True, opacity = 0, column = 3, row = -0.5, orientation = Horizontal }
            , { hidden = True, opacity = 0, column = 4, row = -0.5, orientation = Horizontal }
            , { hidden = True, opacity = 0, column = 0, row = 4.5, orientation = Horizontal }
            , { hidden = True, opacity = 0, column = 1, row = 4.5, orientation = Horizontal }
            , { hidden = True, opacity = 0, column = 2, row = 4.5, orientation = Horizontal }
            , { hidden = True, opacity = 0, column = 3, row = 4.5, orientation = Horizontal }
            , { hidden = True, opacity = 0, column = 4, row = 4.5, orientation = Horizontal }
            , { hidden = True, opacity = 0, column = -0.5, row = 0, orientation = Vertical }
            , { hidden = True, opacity = 0, column = -0.5, row = 1, orientation = Vertical }
            , { hidden = True, opacity = 0, column = -0.5, row = 2, orientation = Vertical }
            , { hidden = True, opacity = 0, column = -0.5, row = 3, orientation = Vertical }
            , { hidden = True, opacity = 0, column = -0.5, row = 4, orientation = Vertical }
            , { hidden = True, opacity = 0, column = 4.5, row = 0, orientation = Vertical }
            , { hidden = True, opacity = 0, column = 4.5, row = 1, orientation = Vertical }
            , { hidden = True, opacity = 0, column = 4.5, row = 2, orientation = Vertical }
            , { hidden = True, opacity = 0, column = 4.5, row = 3, orientation = Vertical }
            ]
      , mouse =
            { position = { x = 0, y = 0 }
            , buttonDown = NoMouseButton
            }
      , drawing = []
      , snappedDrawingPoints = []
      }
    , Cmd.none
    )



---- UPDATE


type Msg
    = Tick Float
    | MoveButtonPressed MoveDirection
    | KeyDown RawKey
    | MouseUpdated Mouse


type MoveDirection
    = MoveRight
    | MoveLeft
    | MoveUp
    | MoveDown


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MoveButtonPressed moveDirection ->
            ( tryStartPlayerMove moveDirection model, Cmd.none )

        KeyDown rawKey ->
            case moveDirectionFromKeyDown rawKey of
                Just moveDirection ->
                    ( tryStartPlayerMove moveDirection model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        Tick deltaTime ->
            ( updatePlayerPosition deltaTime model
                |> returnToOriginIfPathUnclear
                |> finishPlayerMove
                |> updateWallsOpacity deltaTime
            , Cmd.none
            )

        MouseUpdated mouse ->
            ( { model | mouse = Debug.log "mouse" mouse }
                |> updateDrawing
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
                moveDistance =
                    settings.playerMoveSpeed * deltaTime

                column =
                    case currentMove.direction of
                        MoveRight ->
                            model.position.column + moveDistance

                        MoveLeft ->
                            model.position.column - moveDistance

                        _ ->
                            model.position.column

                row =
                    case currentMove.direction of
                        MoveDown ->
                            model.position.row + moveDistance

                        MoveUp ->
                            model.position.row - moveDistance

                        _ ->
                            model.position.row
            in
            { model | clock = clock, position = Position column row }

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
                { model | position = currentMove.origin, currentMove = Nothing }

            else
                { model | position = currentMove.target, currentMove = Nothing }

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
                        |> revealWall

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


pathAheadClear : Model -> Bool
pathAheadClear model =
    case model.currentMove of
        Just currentMove ->
            if wallExistsBetweenPoints model.walls currentMove.origin currentMove.target then
                (numberBetween currentMove.origin.column (currentMove.origin.column + 0.4) model.position.column
                    || numberBetween currentMove.origin.column (currentMove.origin.column - 0.4) model.position.column
                )
                    && (numberBetween currentMove.origin.row (currentMove.origin.row + 0.4) model.position.row
                            || numberBetween currentMove.origin.row (currentMove.origin.row - 0.4) model.position.row
                       )

            else
                True

        Nothing ->
            True


wallExistsBetweenPoints : Walls -> Position -> Position -> Bool
wallExistsBetweenPoints walls pointA pointB =
    let
        midpoint =
            calculateMidpoint pointA pointB

        wall =
            walls |> List.Extra.find (wallIsAtPoint midpoint)
    in
    case wall of
        Just _ ->
            True

        Nothing ->
            False


calculateMidpoint : Position -> Position -> Position
calculateMidpoint pointA pointB =
    { column = (pointA.column + pointB.column) / 2
    , row = (pointA.row + pointB.row) / 2
    }


wallIsAtPoint : Position -> Wall -> Bool
wallIsAtPoint point wall =
    wall.column == point.column && wall.row == point.row


revealWall : Model -> Model
revealWall model =
    case model.currentMove of
        Just currentMove ->
            let
                currentMoveMidpoint =
                    calculateMidpoint currentMove.origin currentMove.target

                walls =
                    model.walls |> List.map (revealWallIfAtPoint currentMoveMidpoint)
            in
            { model | walls = walls }

        Nothing ->
            model


revealWallIfAtPoint : Position -> Wall -> Wall
revealWallIfAtPoint point wall =
    if wall.column == point.column && wall.row == point.row then
        { wall | hidden = False }

    else
        wall


playerWithinMoveBounds : Model -> { direction : MoveDirection, origin : Position, target : Position, reversing : Bool } -> Bool
playerWithinMoveBounds model currentMove =
    let
        columnWithinMoveBounds =
            numberBetween currentMove.origin.column currentMove.target.column model.position.column

        rowWithinMoveBounds =
            numberBetween currentMove.origin.row currentMove.target.row model.position.row
    in
    columnWithinMoveBounds && rowWithinMoveBounds


numberBetween : Float -> Float -> Float -> Bool
numberBetween a b num =
    a <= num && num <= b || b <= num && num <= a


moveDirectionFromKeyDown : RawKey -> Maybe MoveDirection
moveDirectionFromKeyDown rawKey =
    case Keyboard.anyKeyUpper rawKey of
        Just ArrowRight ->
            Just MoveRight

        Just ArrowLeft ->
            Just MoveLeft

        Just ArrowUp ->
            Just MoveUp

        Just ArrowDown ->
            Just MoveDown

        Just (Character "D") ->
            Just MoveRight

        Just (Character "A") ->
            Just MoveLeft

        Just (Character "W") ->
            Just MoveUp

        Just (Character "S") ->
            Just MoveDown

        _ ->
            Nothing


startPlayerMove : MoveDirection -> Model -> Model
startPlayerMove moveDirection model =
    let
        origin =
            { column = model.position.column, row = model.position.row }
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


updateWallsOpacity : Float -> Model -> Model
updateWallsOpacity deltaTime model =
    { model | walls = List.map (updateWallOpacity deltaTime) model.walls }


updateWallOpacity : Float -> Wall -> Wall
updateWallOpacity deltaTime wall =
    let
        revealing =
            if wall.hidden then
                -1

            else
                1

        opacity =
            (wall.opacity + settings.wallOpacitySpeed * deltaTime * revealing)
                |> max 0
                |> min 1
    in
    { wall | opacity = opacity }


updateDrawing : Model -> Model
updateDrawing model =
    let
        drawing =
            case model.mouse.buttonDown of
                LeftMouseButton ->
                    List.concat
                        [ model.drawing
                        , [ model.mouse.position ]
                        ]

                RightMouseButton ->
                    model.drawing

                NoMouseButton ->
                    model.drawing

        snappedDrawingPoints =
            case model.mouse.buttonDown of
                LeftMouseButton ->
                    let
                        mousePosition =
                            { column = model.mouse.position.x |> columnFromX
                            , row = model.mouse.position.y |> rowFromY
                            }

                        nearestGridIntersection =
                            findNearestGridIntersection mousePosition

                        distanceToNearestGridIntersection =
                            distanceBetweenPoints mousePosition nearestGridIntersection
                    in
                    if mousePosition.column < gridSize.columnCount && distanceToNearestGridIntersection < 0.4 then
                        case List.head model.snappedDrawingPoints of
                            Nothing ->
                                [ nearestGridIntersection ]

                            Just previousSnappedDrawingPoint ->
                                if nearestGridIntersection == previousSnappedDrawingPoint then
                                    model.snappedDrawingPoints

                                else
                                    List.concat [ [ nearestGridIntersection ], model.snappedDrawingPoints ]

                    else
                        model.snappedDrawingPoints

                RightMouseButton ->
                    model.snappedDrawingPoints

                NoMouseButton ->
                    model.snappedDrawingPoints
    in
    { model | drawing = drawing, snappedDrawingPoints = snappedDrawingPoints } |> finishDrawing


finishDrawing : Model -> Model
finishDrawing model =
    case model.mouse.buttonDown of
        NoMouseButton ->
            case List.head model.snappedDrawingPoints of
                Just _ ->
                    let
                        listOfMaybeWalls =
                            listMapConsecutively createWall model.snappedDrawingPoints

                        newWalls =
                            listOfMaybeWalls |> Maybe.Extra.values |> Debug.log "New walls"

                        walls =
                            List.concat [ newWalls, model.walls ]
                    in
                    { model | drawing = [], snappedDrawingPoints = [], walls = walls }

                Nothing ->
                    { model | drawing = [], snappedDrawingPoints = [] }

        _ ->
            model


createWall : Position -> Position -> Maybe Wall
createWall pointA pointB =
    if distanceBetweenPoints pointA pointB == 1 then
        if pointA.column == pointB.column then
            let
                orientation =
                    Vertical

                column =
                    pointA.column

                row =
                    (pointA.row + pointB.row) / 2
            in
            Just { hidden = False, opacity = 1, column = column, row = row, orientation = orientation }

        else
            let
                orientation =
                    Horizontal

                column =
                    (pointA.column + pointB.column) / 2

                row =
                    pointA.row
            in
            Just { hidden = False, opacity = 1, column = column, row = row, orientation = orientation }

    else
        Nothing


distanceBetweenPoints : Position -> Position -> Float
distanceBetweenPoints pointA pointB =
    let
        columnDiff =
            pointA.column - pointB.column

        rowDiff =
            pointA.row - pointB.row
    in
    sqrt (columnDiff * columnDiff + rowDiff * rowDiff)


findNearestGridIntersection : Position -> Position
findNearestGridIntersection position =
    { row = position.row |> roundToNearestHalf
    , column = position.column |> roundToNearestHalf
    }


roundToNearestHalf : Float -> Float
roundToNearestHalf n =
    (n + 0.5 |> roundFloat) - 0.5



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.downs KeyDown
        , onAnimationFrameDelta Tick
        ]


updateMouseOn : String -> Html.Styled.Attribute Msg
updateMouseOn eventName =
    let
        decoder =
            decodeMouseMove
                |> Json.Decode.map (\mouse -> MouseUpdated mouse)
                |> Json.Decode.map options

        options message =
            { message = message
            , stopPropagation = False
            , preventDefault = True
            }
    in
    Html.Styled.Events.custom eventName decoder


decodeMouseMove : Json.Decode.Decoder Mouse
decodeMouseMove =
    Json.Decode.map2 Mouse
        (Json.Decode.map2 Coordinate
            (Json.Decode.field "pageX" Json.Decode.float)
            (Json.Decode.field "pageY" Json.Decode.float)
        )
        (Json.Decode.field "buttons" mouseButtonDecoder)


mouseButtonDecoder : Json.Decode.Decoder MouseButton
mouseButtonDecoder =
    Json.Decode.int
        |> Json.Decode.andThen
            (\buttonNum ->
                case buttonNum of
                    1 ->
                        Json.Decode.succeed LeftMouseButton

                    2 ->
                        Json.Decode.succeed RightMouseButton

                    _ ->
                        Json.Decode.succeed NoMouseButton
            )



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



-- Workings:
-- x = gridBorder.left + gridSize.cellWidth * (0.5 + column) ; minus gridBorder.left
-- x - gridBorder.left = gridSize.cellWidth * (0.5 + column) ; divide by gridSize.cellWidth
-- (x - gridBorder.left) / gridSize.cellWidth = 0.5 + column ; minus 0.5
-- (x - gridBorder.left) / gridSize.cellWidth - 0.5 = column


columnFromX : Float -> Float
columnFromX x =
    (x - gridBorder.left) / gridSize.cellWidth - 0.5


rowFromY : Float -> Float
rowFromY y =
    (y - gridBorder.top) / gridSize.cellHeight - 0.5


roundFloat =
    round >> toFloat


viewBoard model =
    div
        [ css [ width (px 640), height (px 480), Css.touchAction Css.none ]
        , updateMouseOn "pointerdown"
        , updateMouseOn "pointerup"
        , updateMouseOn "pointermove"
        ]
        (List.concat
            [ [ viewBackground
              , viewPlayer model
              ]
            , viewWalls model
            , viewDrawing model
            , viewSnappedDrawingPoints model.snappedDrawingPoints
            ]
        )


viewBackground =
    img
        [ src "/assets/images/bg_level.png"
        , css [ width (px 640), height (px 480) ]
        , draggable "false"
        , onContextMenuPreventDefault (Tick 0)
        ]
        []


onContextMenuPreventDefault : msg -> Html.Styled.Attribute msg
onContextMenuPreventDefault msg =
    Html.Styled.Events.preventDefaultOn "contextmenu" (Json.Decode.map alwaysPreventDefault (Json.Decode.succeed msg))


alwaysPreventDefault : msg -> ( msg, Bool )
alwaysPreventDefault msg =
    ( msg, True )


viewPlayer model =
    img
        [ src "/assets/images/man.png"
        , css
            [ position absolute
            , width (px 11)
            , height (px 31)
            , left (px ((model.position.column |> xFromColumn) - (11 / 2) |> roundFloat))
            , top (px ((model.position.row |> yFromRow) - 31 / 2 + 1 |> roundFloat))
            ]
        , draggable "false"
        , onContextMenuPreventDefault (Tick 0)
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
                , opacity (Css.num wall.opacity)
                ]
            , draggable "false"
            , onContextMenuPreventDefault (Tick 0)
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
                , opacity (Css.num wall.opacity)
                ]
            , draggable "false"
            , onContextMenuPreventDefault (Tick 0)
            ]
            []


listMapConsecutively : (a -> a -> b) -> List a -> List b
listMapConsecutively mapFn list =
    case List.head list of
        Just first ->
            case List.tail list of
                Just tail ->
                    case List.head tail of
                        Just second ->
                            List.concat
                                [ [ mapFn first second ]
                                , listMapConsecutively mapFn tail
                                ]

                        Nothing ->
                            []

                Nothing ->
                    []

        Nothing ->
            []


viewDrawing model =
    let
        drawnSegments =
            listMapConsecutively viewDrawingSegment model.drawing
    in
    [ div [] drawnSegments ]


viewDrawingSegment coordA coordB =
    let
        x1 =
            coordA.x

        x2 =
            coordB.x

        y1 =
            coordA.y

        y2 =
            coordB.y

        xm =
            (x1 + x2) / 2

        ym =
            (y1 + y2) / 2

        ht =
            y2 - y1

        w =
            x2 - x1

        h =
            sqrt (ht * ht + w * w)

        l =
            xm - h / 2

        t =
            ym

        angle =
            atan2 ht w
    in
    div
        [ css
            [ position absolute
            , left (px l)
            , top (px t)
            , width (px h)
            , height (px 3)
            , backgroundColor (hex "#000")
            , transform (rotate (rad angle))
            ]
        ]
        []


viewSnappedDrawingPoints snappedDrawingPoints =
    List.map viewSnappedDrawingPoint snappedDrawingPoints


viewSnappedDrawingPoint snappedDrawingPoint =
    div
        [ css
            [ position absolute
            , width (px 4)
            , height (px 4)
            , left (px ((snappedDrawingPoint.column |> xFromColumn) - (4 / 2) |> roundFloat))
            , top (px ((snappedDrawingPoint.row |> yFromRow) - (4 / 2) |> roundFloat))
            , backgroundColor (hex "#c00")
            ]
        , draggable "false"
        , onContextMenuPreventDefault (Tick 0)
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
        , div [] [ text "or use WASD / arrow keys" ]
        ]


viewArrowButton moveDirection buttonText =
    button
        [ css
            [ width (px 50)
            , height (px 50)
            , fontSize (px 30)
            ]
        , onClick (MoveButtonPressed moveDirection)
        ]
        [ text buttonText ]


viewGithubLink =
    div [] [ a [ href "https://github.com/ZimbiX/pathfinder-elm" ] [ text "GitHub" ] ]
