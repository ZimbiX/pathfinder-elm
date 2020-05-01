module Main exposing (Model, Msg(..), init, main, update, view)

import Basics.Extra exposing (fractionalModBy)
import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Css exposing (absolute, backgroundColor, border3, height, hex, left, opacity, pct, px, rad, rotate, solid, top, transform, width)
import Css.Animations
import Css.Transitions
import Debug
import Html exposing (Html)
import Html.Events.Extra
import Html.Styled exposing (a, button, div, img, text, toUnstyled)
import Html.Styled.Attributes exposing (css, draggable, href, src)
import Html.Styled.Events exposing (on, onClick)
import Html.Styled.Keyed as Keyed
import Html.Styled.Lazy exposing (lazy, lazy2)
import Http
import Json.Decode
import Json.Encode
import Keyboard exposing (Key(..), RawKey)
import Keyboard.Arrows
import List.Extra
import Maybe.Extra
import Process
import Task
import Url



-- SETTINGS


settings =
    { playerMoveSpeed = 0.007
    , wallOpacitySpeed = 0.008
    , boardZoom = 2
    , boardWidth = 640
    , boardHeight = 480
    , mazeSwitching =
        { delaySeconds = 0.2
        , animationDurationSeconds = 0.8
        }
    }


gridSize =
    { rowCount = 11
    , columnCount = 11
    , cellWidth = 32
    , cellHeight = 32
    }


gridBorder =
    { top = (settings.boardHeight - (gridSize.rowCount * gridSize.cellHeight)) / 2
    , left = (settings.boardWidth - (gridSize.columnCount * gridSize.cellWidth)) / 2
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
    { mazes : ( Maze, Maze )
    , mouse : Mouse
    , drawing : Drawing
    , snappedDrawingPoints : SnappedDrawingPoints
    , popup : Maybe Popup
    , switchingMaze : SwitchingMazeState
    , gameStateVersion : Int
    , queuedEventsForApplication : List VersionedBackendEvent
    , eventsQueuedForSubmission : List VersionedBackendEvent
    }


type alias Maze =
    { position : Position
    , currentMove : CurrentMove
    , walls : Walls
    , golds : Golds
    , stage : Stage
    , pathTravelled : PathTravelled
    }


type alias Walls =
    List Wall


type alias Golds =
    List Position


type alias Gold =
    Position


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


type alias WallBackend =
    { column : Float
    , row : Float
    , orientation : Orientation
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
    | MiddleMouseButton
    | NoMouseButton


type alias Drawing =
    List Coordinate


type alias SnappedDrawingPoints =
    List Position


type alias PathTravelled =
    List Position


type alias Coordinate =
    { x : Float, y : Float }


type Stage
    = DrawingStage
    | PlayingStage
    | FirstWinStage


type alias Popup =
    { messageLines : List String }


type SwitchingMazeState
    = SwitchingMaze Float
    | NotSwitchingMaze


type alias MazeBackend =
    { walls : List WallBackend
    , golds : List Gold
    }


type BackendEvent
    = MazeDrawn MazeBackend
    | MoveLeftBackendEvent
    | MoveRightBackendEvent
    | MoveUpBackendEvent
    | MoveDownBackendEvent


type alias VersionedBackendEvent =
    { event : BackendEvent
    , version : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { mazes =
            ( initialMaze, initialMaze )
      , mouse =
            { position = { x = 0, y = 0 }
            , buttonDown = NoMouseButton
            }
      , drawing = []
      , snappedDrawingPoints = []
      , popup = Nothing
      , switchingMaze = NotSwitchingMaze
      , gameStateVersion = 0
      , queuedEventsForApplication = []
      , eventsQueuedForSubmission = []
      }
    , requestNewEvents 0
    )


initialMaze =
    { position = { column = 0, row = 0 }
    , currentMove = Nothing
    , walls = []
    , golds = []
    , stage = DrawingStage
    , pathTravelled = []
    }



---- UPDATE


type Msg
    = Tick Float
    | MoveButtonPressed MoveDirection
    | KeyDown RawKey
    | MouseUpdated Mouse
    | DoneButtonPressed
    | DismissPopup
    | RequestNewEventsFromBackend
    | GotEventsFromBackend (Result Http.Error String)
    | SentEventToBackend (Result Http.Error String)


type MoveDirection
    = MoveRight
    | MoveLeft
    | MoveUp
    | MoveDown


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MoveButtonPressed moveDirection ->
            model
                |> tryStartPlayerMove moveDirection
                |> submitQueuedEvents

        KeyDown rawKey ->
            case moveDirectionFromKeyDown rawKey of
                Just moveDirection ->
                    model
                        |> tryStartPlayerMove moveDirection
                        |> submitQueuedEvents

                Nothing ->
                    model
                        |> completeMazeDrawingIfEnterPressed rawKey
                        |> dismissPopupIfEnterPressed rawKey
                        |> switchMazeIfMPressed rawKey
                        |> submitQueuedEvents

        Tick deltaTime ->
            ( model
                |> applyNextEventFromServerIfReady
                |> updatePlayerPosition deltaTime
                |> returnToOriginIfPathUnclear
                |> finishPlayerMove
                |> endGameIfWon
                |> updateWallsOpacity deltaTime
                |> keepSwitchingMazes deltaTime
                |> swapMazesIfFinishedSwitching
            , Cmd.none
            )

        MouseUpdated unscaledMouse ->
            ( model
                |> updateMouseFromUnscaled unscaledMouse
                |> updateDrawing
                |> mouseProcessorForStageInteractions (model.mazes |> Tuple.first).stage
                |> clearDrawingIfFinished
            , Cmd.none
            )

        DoneButtonPressed ->
            model
                |> completeMazeDrawing
                |> submitQueuedEvents

        DismissPopup ->
            ( model
                |> dismissPopup
            , Cmd.none
            )

        RequestNewEventsFromBackend ->
            ( model, requestNewEvents model.gameStateVersion )

        GotEventsFromBackend result ->
            case result of
                Ok eventsResponse ->
                    case eventsResponse |> Json.Decode.decodeString eventsDecoder of
                        Ok events ->
                            model
                                |> queueNewEventsForApplication (events |> Debug.log "Decoded new events")

                        Err decodeErr ->
                            ( eventsResponse, decodeErr ) |> Debug.log "Error decoding received new events" |> (\_ -> ( model, Cmd.none ))

                Err requestErr ->
                    requestErr |> Debug.log "Error from requesting new events" |> (\_ -> ( model, Cmd.none ))

        SentEventToBackend result ->
            case result of
                Ok submissionResponse ->
                    submissionResponse |> Debug.log "Submitted event" |> (\_ -> ( model, Cmd.none ))

                Err requestErr ->
                    requestErr |> Debug.log "Error submitting event" |> (\_ -> ( model, Cmd.none ))


applyNextEventFromServerIfReady : Model -> Model
applyNextEventFromServerIfReady model =
    case List.Extra.uncons model.queuedEventsForApplication |> Debug.log "applyNextEventFromServerIfReady" of
        Just ( firstEvent, laterEvents ) ->
            let
                moveIfReady direction =
                    if playerCanStartMove model then
                        model
                            |> startPlayerMove direction
                            |> (\newModel -> { newModel | queuedEventsForApplication = laterEvents })

                    else
                        model
            in
            case firstEvent.event of
                MazeDrawn mazeBackend ->
                    { model
                        | queuedEventsForApplication = laterEvents
                        , mazes =
                            model.mazes
                                |> Tuple.mapFirst
                                    (\maze ->
                                        { maze
                                            | golds = mazeBackend.golds
                                            , walls = mazeBackend.walls |> wallsFromBackendWalls
                                        }
                                    )
                    }
                        |> completeMazeDrawing
                        |> swapMazes

                MoveLeftBackendEvent ->
                    moveIfReady MoveLeft

                MoveRightBackendEvent ->
                    moveIfReady MoveRight

                MoveUpBackendEvent ->
                    moveIfReady MoveUp

                MoveDownBackendEvent ->
                    moveIfReady MoveDown

        --unhandledEvent ->
        --    Debug.log "Unhandled application of event" unhandledEvent
        --        |> (\_ -> model)
        --        |> (\newModel -> { newModel | queuedEventsForApplication = laterEvents })
        Nothing ->
            model


wallsFromBackendWalls : List WallBackend -> List Wall
wallsFromBackendWalls backendWalls =
    List.map
        (\backEndWall ->
            { column = backEndWall.column
            , row = backEndWall.row
            , orientation = backEndWall.orientation
            , hidden = True
            , opacity = 0
            }
        )
        backendWalls


backendWallsFromWalls : List Wall -> List WallBackend
backendWallsFromWalls walls =
    List.map
        (\wall ->
            { column = wall.column
            , row = wall.row
            , orientation = wall.orientation
            }
        )
        walls


queueNewEventsForApplication : List VersionedBackendEvent -> Model -> ( Model, Cmd Msg )
queueNewEventsForApplication events model =
    let
        queuedEventsForApplication =
            List.concat [ model.queuedEventsForApplication, events ]
                |> List.sortBy (\event -> event.version)
                |> Debug.log "updated queuedEventsForApplication"
    in
    case List.Extra.last events of
        Just latestEvent ->
            ( { model | gameStateVersion = latestEvent.version, queuedEventsForApplication = queuedEventsForApplication }
              --, Cmd.none
            , Process.sleep 100 |> Task.perform (\_ -> RequestNewEventsFromBackend)
            )

        Nothing ->
            ( model
              --, Cmd.none
            , Process.sleep 1000 |> Task.perform (\_ -> RequestNewEventsFromBackend)
            )


requestNewEvents afterVersion =
    Http.get
        { url = "http://www.zimbico.net/pathfinder-elm-backend/pathfinder-elm-backend.php?id=d&after=" ++ String.fromInt afterVersion
        , expect = Http.expectString GotEventsFromBackend
        }


backendEventDecoder : Json.Decode.Decoder BackendEvent
backendEventDecoder =
    let
        decodeMoveLeft =
            exactMatch (Json.Decode.field "name" Json.Decode.string) "MoveLeft" (Json.Decode.succeed MoveLeftBackendEvent)

        decodeMoveRight =
            exactMatch (Json.Decode.field "name" Json.Decode.string) "MoveRight" (Json.Decode.succeed MoveRightBackendEvent)

        decodeMoveUp =
            exactMatch (Json.Decode.field "name" Json.Decode.string) "MoveUp" (Json.Decode.succeed MoveUpBackendEvent)

        decodeMoveDown =
            exactMatch (Json.Decode.field "name" Json.Decode.string) "MoveDown" (Json.Decode.succeed MoveDownBackendEvent)

        decodeMazeDrawn =
            exactMatch
                (Json.Decode.field "name" Json.Decode.string)
                "MazeDrawn"
                (Json.Decode.field "data"
                    (Json.Decode.map
                        MazeDrawn
                        (Json.Decode.map2
                            MazeBackend
                            (Json.Decode.field "walls"
                                (Json.Decode.list
                                    (Json.Decode.map3
                                        WallBackend
                                        (Json.Decode.field "column" Json.Decode.float)
                                        (Json.Decode.field "row" Json.Decode.float)
                                        (Json.Decode.field "orientation" orientationDecoder)
                                    )
                                )
                            )
                            (Json.Decode.field "golds"
                                (Json.Decode.list
                                    (Json.Decode.map2
                                        Position
                                        (Json.Decode.field "column" Json.Decode.float)
                                        (Json.Decode.field "row" Json.Decode.float)
                                    )
                                )
                            )
                        )
                    )
                )
    in
    Json.Decode.oneOf
        [ decodeMoveLeft
        , decodeMoveRight
        , decodeMoveUp
        , decodeMoveDown
        , decodeMazeDrawn
        ]


exactMatch : Json.Decode.Decoder String -> String -> Json.Decode.Decoder a -> Json.Decode.Decoder a
exactMatch matchDecoder match dec =
    matchDecoder
        |> Json.Decode.andThen
            (\str ->
                if str == match then
                    dec

                else
                    Json.Decode.fail <| "[exactMatch] tgt: " ++ match ++ " /= " ++ str
            )


orientationDecoder : Json.Decode.Decoder Orientation
orientationDecoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\orientation ->
                case orientation of
                    "Vertical" ->
                        Json.Decode.succeed Vertical

                    "Horizontal" ->
                        Json.Decode.succeed Horizontal

                    _ ->
                        Json.Decode.fail <| "Orientation decoding failed: " ++ orientation
            )


eventDecoder : Json.Decode.Decoder VersionedBackendEvent
eventDecoder =
    Json.Decode.map2 VersionedBackendEvent
        (Json.Decode.field "event" backendEventDecoder)
        (Json.Decode.field "version" Json.Decode.int)


eventsDecoder : Json.Decode.Decoder (List VersionedBackendEvent)
eventsDecoder =
    Json.Decode.list eventDecoder


moveEventEncoder : MoveDirection -> Json.Encode.Value
moveEventEncoder direction =
    let
        name =
            case direction of
                MoveLeft ->
                    "MoveLeft"

                MoveRight ->
                    "MoveRight"

                MoveUp ->
                    "MoveUp"

                MoveDown ->
                    "MoveDown"
    in
    Json.Encode.object
        [ ( "name", Json.Encode.string name )
        , ( "data", Json.Encode.null )
        ]


mazeDrawnEventEncoder : MazeBackend -> Json.Encode.Value
mazeDrawnEventEncoder mazeBackend =
    Json.Encode.object
        [ ( "name", Json.Encode.string "MazeDrawn" )
        , ( "data"
          , Json.Encode.object
                [ ( "walls"
                  , Json.Encode.list
                        (\wall ->
                            let
                                orientation =
                                    case wall.orientation of
                                        Horizontal ->
                                            "Horizontal"

                                        Vertical ->
                                            "Vertical"
                            in
                            Json.Encode.object
                                [ ( "row", Json.Encode.float wall.row )
                                , ( "column", Json.Encode.float wall.column )
                                , ( "orientation", Json.Encode.string orientation )
                                ]
                        )
                        mazeBackend.walls
                  )
                , ( "golds"
                  , Json.Encode.list
                        (\gold ->
                            Json.Encode.object
                                [ ( "row", Json.Encode.float gold.row )
                                , ( "column", Json.Encode.float gold.column )
                                ]
                        )
                        mazeBackend.golds
                  )
                ]
          )
        ]


formUrlencoded : List ( String, String ) -> String
formUrlencoded object =
    object
        |> List.map
            (\( name, value ) ->
                Url.percentEncode name
                    ++ "="
                    ++ Url.percentEncode value
            )
        |> String.join "&"


submitEvent : VersionedBackendEvent -> Cmd Msg
submitEvent versionedEvent =
    let
        encodedEvent =
            case versionedEvent.event of
                MazeDrawn mazeBackend ->
                    mazeDrawnEventEncoder mazeBackend

                MoveLeftBackendEvent ->
                    moveEventEncoder MoveLeft

                MoveRightBackendEvent ->
                    moveEventEncoder MoveRight

                MoveUpBackendEvent ->
                    moveEventEncoder MoveUp

                MoveDownBackendEvent ->
                    moveEventEncoder MoveDown

        body =
            [ ( "id", "d" )
            , ( "version", String.fromInt versionedEvent.version )
            , ( "event", Json.Encode.encode 0 encodedEvent )
            ]
    in
    Http.post
        { url = "http://www.zimbico.net/pathfinder-elm-backend/pathfinder-elm-backend.php"
        , body = body |> formUrlencoded |> Http.stringBody "application/x-www-form-urlencoded"
        , expect = Http.expectString SentEventToBackend
        }


submitQueuedEvents : Model -> ( Model, Cmd Msg )
submitQueuedEvents model =
    let
        -- TODO: Handle version iterating to allow submitting multiple events
        versionedEvents =
            model.eventsQueuedForSubmission

        requests =
            List.map submitEvent versionedEvents

        newVersion =
            model.gameStateVersion + List.length versionedEvents
    in
    ( { model
        | gameStateVersion = newVersion
        , eventsQueuedForSubmission = []
      }
    , Cmd.batch requests
    )


mouseProcessorForStageInteractions : Stage -> (Model -> Model)
mouseProcessorForStageInteractions stage =
    case stage of
        DrawingStage ->
            updateSnappedDrawingPoints
                >> createGold
                >> deleteGold
                >> deleteWall
                >> createWallsFromFinishedDrawing

        PlayingStage ->
            tryStartPlayerMoveFromSwipe

        FirstWinStage ->
            \x -> x


updateMouseFromUnscaled : Mouse -> Model -> Model
updateMouseFromUnscaled unscaledMouse model =
    { model | mouse = { unscaledMouse | position = unscaledMouse.position |> zoomCoordinate } }


zoomCoordinate : Coordinate -> Coordinate
zoomCoordinate coord =
    { x = coord.x / settings.boardZoom
    , y = coord.y / settings.boardZoom
    }


updatePlayerPosition : Float -> Model -> Model
updatePlayerPosition deltaTime model =
    case (model.mazes |> Tuple.first).currentMove of
        Just currentMove ->
            let
                moveDistance =
                    settings.playerMoveSpeed * deltaTime

                column =
                    case currentMove.direction of
                        MoveRight ->
                            (model.mazes |> Tuple.first).position.column + moveDistance

                        MoveLeft ->
                            (model.mazes |> Tuple.first).position.column - moveDistance

                        _ ->
                            (model.mazes |> Tuple.first).position.column

                row =
                    case currentMove.direction of
                        MoveDown ->
                            (model.mazes |> Tuple.first).position.row + moveDistance

                        MoveUp ->
                            (model.mazes |> Tuple.first).position.row - moveDistance

                        _ ->
                            (model.mazes |> Tuple.first).position.row

                position =
                    Position column row
            in
            { model
                | mazes = model.mazes |> Tuple.mapFirst (\maze -> { maze | position = position })
            }

        Nothing ->
            model


tryStartPlayerMoveFromSwipe : Model -> Model
tryStartPlayerMoveFromSwipe model =
    case model.mouse.buttonDown of
        NoMouseButton ->
            let
                maybeSwipeDirection =
                    model.drawing |> List.map positionFromCoordinate |> getSwipeDirection
            in
            case maybeSwipeDirection of
                Just swipeDirection ->
                    model |> tryStartPlayerMove swipeDirection

                Nothing ->
                    model

        _ ->
            model


getSwipeDirection : List Position -> Maybe MoveDirection
getSwipeDirection drawingPositions =
    case List.head drawingPositions of
        Just start ->
            case List.Extra.last drawingPositions of
                Just finish ->
                    if distanceBetweenPoints start finish > 1 then
                        let
                            angle =
                                degreesFromRadians
                                    (atan2
                                        (finish.row - start.row)
                                        (finish.column - start.column)
                                    )
                                    |> Debug.log "angle"

                            angleMagnet =
                                30

                            angleIsNear =
                                \angleToBeNear -> numberBetween (angleToBeNear - angleMagnet) (angleToBeNear + angleMagnet) angle
                        in
                        if angleIsNear 0 || angleIsNear 360 then
                            Just MoveRight

                        else if angleIsNear 90 then
                            Just MoveDown

                        else if angleIsNear 180 then
                            Just MoveLeft

                        else if angleIsNear 270 then
                            Just MoveUp

                        else
                            Nothing

                    else
                        Nothing

                Nothing ->
                    Nothing

        Nothing ->
            Nothing


degreesFromRadians : Float -> Float
degreesFromRadians angleRadians =
    fractionalModBy 360 (360 + (angleRadians |> Debug.log "radians") * (180 / pi))


playerCanStartMove : Model -> Bool
playerCanStartMove model =
    case (model.mazes |> Tuple.first).currentMove of
        Just _ ->
            False

        Nothing ->
            case (model.mazes |> Tuple.first).stage of
                PlayingStage ->
                    case model.switchingMaze of
                        SwitchingMaze _ ->
                            False

                        NotSwitchingMaze ->
                            True

                DrawingStage ->
                    False

                FirstWinStage ->
                    False


tryStartPlayerMove : MoveDirection -> Model -> Model
tryStartPlayerMove moveDirection model =
    if playerCanStartMove model then
        model |> startPlayerMove moveDirection

    else
        model


finishPlayerMove : Model -> Model
finishPlayerMove model =
    case (model.mazes |> Tuple.first).currentMove of
        Just currentMove ->
            if playerWithinMoveBounds model currentMove then
                model

            else if currentMove.reversing then
                { model
                    | mazes =
                        model.mazes
                            |> Tuple.mapFirst
                                (\maze ->
                                    { maze
                                        | position = currentMove.origin
                                        , currentMove = Nothing
                                    }
                                )
                }
                    |> startSwitchingMazesIfOtherMazeNotWon

            else
                let
                    pathTravelled =
                        List.concat [ [ currentMove.target ], (model.mazes |> Tuple.first).pathTravelled ]
                in
                { model
                    | mazes =
                        model.mazes
                            |> Tuple.mapFirst
                                (\maze ->
                                    { maze
                                        | position = currentMove.target
                                        , currentMove = Nothing
                                        , pathTravelled = pathTravelled
                                    }
                                )
                }

        Nothing ->
            model


returnToOriginIfPathUnclear : Model -> Model
returnToOriginIfPathUnclear model =
    if pathAheadClear model then
        model

    else
        case (model.mazes |> Tuple.first).currentMove of
            Just currentMove ->
                if currentMove.reversing then
                    model

                else
                    let
                        returnMove =
                            Just { currentMove | reversing = True, direction = oppositeDirection currentMove.direction }
                    in
                    { model | mazes = model.mazes |> Tuple.mapFirst (\maze -> { maze | currentMove = returnMove }) }
                        |> revealHitWall

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
    case (model.mazes |> Tuple.first).currentMove of
        Just currentMove ->
            if not (withinBoard (model.mazes |> Tuple.first).position) then
                False

            else if wallExistsBetweenPoints (model.mazes |> Tuple.first).walls currentMove.origin currentMove.target then
                (numberBetween currentMove.origin.column (currentMove.origin.column + 0.4) (model.mazes |> Tuple.first).position.column
                    || numberBetween currentMove.origin.column (currentMove.origin.column - 0.4) (model.mazes |> Tuple.first).position.column
                )
                    && (numberBetween currentMove.origin.row (currentMove.origin.row + 0.4) (model.mazes |> Tuple.first).position.row
                            || numberBetween currentMove.origin.row (currentMove.origin.row - 0.4) (model.mazes |> Tuple.first).position.row
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


revealHitWall : Model -> Model
revealHitWall model =
    case (model.mazes |> Tuple.first).currentMove of
        Just currentMove ->
            let
                currentMoveMidpoint =
                    calculateMidpoint currentMove.origin currentMove.target

                walls =
                    (model.mazes |> Tuple.first).walls |> List.map (revealWallIfAtPoint currentMoveMidpoint)
            in
            { model | mazes = model.mazes |> Tuple.mapFirst (\maze -> { maze | walls = walls }) }

        Nothing ->
            model


revealWallIfAtPoint : Position -> Wall -> Wall
revealWallIfAtPoint point wall =
    if wall.column == point.column && wall.row == point.row then
        { wall | hidden = False }

    else
        wall


completeMazeDrawingIfEnterPressed : RawKey -> Model -> Model
completeMazeDrawingIfEnterPressed rawKey model =
    case Keyboard.anyKeyUpper rawKey of
        Just Enter ->
            model |> completeMazeDrawing

        _ ->
            model


dismissPopupIfEnterPressed : RawKey -> Model -> Model
dismissPopupIfEnterPressed rawKey model =
    case Keyboard.anyKeyUpper rawKey of
        Just Enter ->
            model |> dismissPopup

        _ ->
            model


dismissPopup : Model -> Model
dismissPopup model =
    let
        modelDismissed =
            { model | popup = Nothing }

        inactiveMaze =
            Tuple.second model.mazes
    in
    case inactiveMaze.stage of
        DrawingStage ->
            modelDismissed

        PlayingStage ->
            modelDismissed |> startSwitchingMazes

        FirstWinStage ->
            modelDismissed


switchMazeIfMPressed : RawKey -> Model -> Model
switchMazeIfMPressed rawKey model =
    case Keyboard.anyKeyUpper rawKey of
        Just (Character "M") ->
            model |> startSwitchingMazes

        _ ->
            model


startSwitchingMazesIfOtherMazeNotWon : Model -> Model
startSwitchingMazesIfOtherMazeNotWon model =
    let
        inactiveMaze =
            model.mazes |> Tuple.second
    in
    case inactiveMaze.stage of
        DrawingStage ->
            model |> startSwitchingMazes

        PlayingStage ->
            model |> startSwitchingMazes

        FirstWinStage ->
            model


startSwitchingMazes : Model -> Model
startSwitchingMazes model =
    let
        initiallyDelayedProgressFraction =
            -settings.mazeSwitching.delaySeconds
                * (1 / settings.mazeSwitching.animationDurationSeconds)
    in
    { model | switchingMaze = SwitchingMaze initiallyDelayedProgressFraction }


keepSwitchingMazes : Float -> Model -> Model
keepSwitchingMazes deltaTime model =
    case model.switchingMaze of
        SwitchingMaze progressFraction ->
            { model | switchingMaze = SwitchingMaze (min (progressFraction + ((deltaTime / 1000) / settings.mazeSwitching.animationDurationSeconds)) 1) }

        NotSwitchingMaze ->
            model


swapMazesIfFinishedSwitching : Model -> Model
swapMazesIfFinishedSwitching model =
    case model.switchingMaze of
        SwitchingMaze progressFraction ->
            if progressFraction == 1 then
                model |> swapMazes

            else
                model

        NotSwitchingMaze ->
            model


swapMazes : Model -> Model
swapMazes model =
    { model
        | switchingMaze = NotSwitchingMaze
        , mazes =
            ( model.mazes |> Tuple.second
            , model.mazes |> Tuple.first
            )
    }


hideAllWalls : Walls -> Walls
hideAllWalls walls =
    walls |> List.map hideWall


hideWall : Wall -> Wall
hideWall wall =
    { wall | hidden = True, opacity = 0 }


revealAllWalls : Walls -> Walls
revealAllWalls walls =
    walls |> List.map revealWall


revealWall : Wall -> Wall
revealWall wall =
    { wall | hidden = False, opacity = 1 }


playerWithinMoveBounds : Model -> { direction : MoveDirection, origin : Position, target : Position, reversing : Bool } -> Bool
playerWithinMoveBounds model currentMove =
    let
        columnWithinMoveBounds =
            numberBetween currentMove.origin.column currentMove.target.column (model.mazes |> Tuple.first).position.column

        rowWithinMoveBounds =
            numberBetween currentMove.origin.row currentMove.target.row (model.mazes |> Tuple.first).position.row
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
        activeMaze =
            model.mazes |> Tuple.first

        origin =
            { column = activeMaze.position.column
            , row = activeMaze.position.row
            }

        moveToTarget target backendEvent =
            let
                newMove =
                    Just
                        { direction = moveDirection
                        , origin = origin
                        , target = target
                        , reversing = False
                        }

                versionedEvent =
                    { event = backendEvent, version = model.gameStateVersion + List.length model.eventsQueuedForSubmission + 1 }
            in
            { model
                | mazes = model.mazes |> Tuple.mapFirst (\maze -> { maze | currentMove = newMove })
                , eventsQueuedForSubmission = List.concat [ model.eventsQueuedForSubmission, [ versionedEvent ] ]
            }
    in
    case moveDirection of
        MoveRight ->
            moveToTarget { origin | column = origin.column + 1 } MoveRightBackendEvent

        MoveLeft ->
            moveToTarget { origin | column = origin.column - 1 } MoveLeftBackendEvent

        MoveUp ->
            moveToTarget { origin | row = origin.row - 1 } MoveUpBackendEvent

        MoveDown ->
            moveToTarget { origin | row = origin.row + 1 } MoveDownBackendEvent


updateWallsOpacity : Float -> Model -> Model
updateWallsOpacity deltaTime model =
    let
        activeMaze =
            model.mazes |> Tuple.first

        walls =
            List.map (updateWallOpacity deltaTime) activeMaze.walls
    in
    { model | mazes = model.mazes |> Tuple.mapFirst (\maze -> { maze | walls = walls }) }


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
    -- The drawing is required for detecting taps/swipes, so we need to update the drawing at all times, except when all input should be disabled
    case model.switchingMaze of
        SwitchingMaze _ ->
            model |> clearDrawing

        NotSwitchingMaze ->
            model |> addMousePositionToDrawingIfMouseDown


addMousePositionToDrawingIfMouseDown : Model -> Model
addMousePositionToDrawingIfMouseDown model =
    let
        drawing =
            case model.mouse.buttonDown of
                LeftMouseButton ->
                    List.concat
                        [ model.drawing
                        , [ model.mouse.position ]
                        ]

                _ ->
                    model.drawing
    in
    { model | drawing = drawing }


updateSnappedDrawingPoints : Model -> Model
updateSnappedDrawingPoints model =
    let
        snappedDrawingPoints =
            case model.mouse.buttonDown of
                LeftMouseButton ->
                    let
                        mousePosition =
                            model.mouse.position |> positionFromCoordinate

                        nearestGridIntersection =
                            findNearestGridIntersection mousePosition

                        distanceToNearestGridIntersection =
                            distanceBetweenPoints mousePosition nearestGridIntersection
                    in
                    if withinBoard nearestGridIntersection && distanceToNearestGridIntersection < 0.4 then
                        addGridIntersectionToDrawingWithInterpolation model.snappedDrawingPoints nearestGridIntersection

                    else
                        model.snappedDrawingPoints

                _ ->
                    model.snappedDrawingPoints
    in
    { model | snappedDrawingPoints = snappedDrawingPoints }


addGridIntersectionToDrawingWithInterpolation : SnappedDrawingPoints -> Position -> SnappedDrawingPoints
addGridIntersectionToDrawingWithInterpolation snappedDrawingPoints gridIntersection =
    case List.head snappedDrawingPoints of
        Nothing ->
            [ gridIntersection ]

        Just previousSnappedDrawingPoint ->
            if gridIntersection == previousSnappedDrawingPoint then
                snappedDrawingPoints

            else if gridIntersection.column == previousSnappedDrawingPoint.column then
                let
                    interpolatedRows =
                        rangeFloatByIncrementDirected previousSnappedDrawingPoint.row gridIntersection.row

                    interpolatedPoints =
                        interpolatedRows |> List.map (\row -> { gridIntersection | row = row })
                in
                case List.tail interpolatedPoints of
                    Just pendingInterpolatedPoints ->
                        List.concat [ pendingInterpolatedPoints |> List.reverse, snappedDrawingPoints ]

                    Nothing ->
                        snappedDrawingPoints

            else if gridIntersection.row == previousSnappedDrawingPoint.row then
                let
                    interpolatedColumns =
                        rangeFloatByIncrementDirected previousSnappedDrawingPoint.column gridIntersection.column

                    interpolatedPoints =
                        interpolatedColumns |> List.map (\column -> { gridIntersection | column = column })
                in
                case List.tail interpolatedPoints of
                    Just pendingInterpolatedPoints ->
                        List.concat [ pendingInterpolatedPoints |> List.reverse, snappedDrawingPoints ]

                    Nothing ->
                        snappedDrawingPoints

            else
                -- Diagonal; can't interpolate
                List.concat [ [ gridIntersection ], snappedDrawingPoints ]


rangeFloatByIncrementDirected : Float -> Float -> List Float
rangeFloatByIncrementDirected start end =
    if start < end then
        rangeFloatByIncrement start end

    else
        rangeFloatByIncrement end start |> List.reverse


rangeFloatByIncrement : Float -> Float -> List Float
rangeFloatByIncrement start end =
    let
        offset =
            fractionalModBy 1 start
    in
    List.range (floor start) (floor end)
        |> List.map (\n -> toFloat n + offset)


createWallsFromFinishedDrawing : Model -> Model
createWallsFromFinishedDrawing model =
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
                            List.concat [ newWalls, (model.mazes |> Tuple.first).walls ]
                    in
                    { model | mazes = model.mazes |> Tuple.mapFirst (\maze -> { maze | walls = walls }) }

                Nothing ->
                    model

        _ ->
            model


clearDrawingIfFinished : Model -> Model
clearDrawingIfFinished model =
    case model.mouse.buttonDown of
        NoMouseButton ->
            model |> clearDrawing

        _ ->
            model


clearDrawing : Model -> Model
clearDrawing model =
    { model | drawing = [], snappedDrawingPoints = [] }


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


findNearestGridCenter : Position -> Position
findNearestGridCenter position =
    { row = position.row |> roundFloat
    , column = position.column |> roundFloat
    }


findNearestGridIntersection : Position -> Position
findNearestGridIntersection position =
    { row = position.row |> roundToNearestHalf
    , column = position.column |> roundToNearestHalf
    }


roundToNearestHalf : Float -> Float
roundToNearestHalf n =
    (n + 0.5 |> roundFloat) - 0.5


createGold : Model -> Model
createGold model =
    if finishedCellTap model then
        let
            nearestGridCenter =
                findNearestGridCenter mousePosition

            mousePosition =
                model.mouse.position |> positionFromCoordinate
        in
        if withinBoard nearestGridCenter then
            let
                golds =
                    List.concat [ [ nearestGridCenter ], (model.mazes |> Tuple.first).golds ]
            in
            { model | mazes = model.mazes |> Tuple.mapFirst (\maze -> { maze | golds = golds }) }

        else
            model

    else
        model


deleteGold : Model -> Model
deleteGold model =
    case model.mouse.buttonDown of
        RightMouseButton ->
            let
                golds =
                    List.filter (itemIsNotUnderPointer model.mouse) (model.mazes |> Tuple.first).golds
            in
            { model | mazes = model.mazes |> Tuple.mapFirst (\maze -> { maze | golds = golds }) }

        _ ->
            model


deleteWall : Model -> Model
deleteWall model =
    case model.mouse.buttonDown of
        RightMouseButton ->
            let
                walls =
                    List.filter (wallIsNotUnderPointer model.mouse) (model.mazes |> Tuple.first).walls
            in
            { model | mazes = model.mazes |> Tuple.mapFirst (\maze -> { maze | walls = walls }) }

        _ ->
            model


wallIsNotUnderPointer : Mouse -> Wall -> Bool
wallIsNotUnderPointer mouse wall =
    let
        wallPosition =
            { column = wall.column, row = wall.row }
    in
    not (itemIsUnderPointer mouse wallPosition)


itemIsNotUnderPointer : Mouse -> Position -> Bool
itemIsNotUnderPointer mouse itemPosition =
    not (itemIsUnderPointer mouse itemPosition)


itemIsUnderPointer : Mouse -> Position -> Bool
itemIsUnderPointer mouse itemPosition =
    let
        magnetDistance =
            0.3

        mousePosition =
            mouse.position |> positionFromCoordinate
    in
    (mousePosition.row - magnetDistance < itemPosition.row && itemPosition.row < mousePosition.row + magnetDistance)
        && (mousePosition.column - magnetDistance < itemPosition.column && itemPosition.column < mousePosition.column + magnetDistance)


finishedCellTap : Model -> Bool
finishedCellTap model =
    case model.mouse.buttonDown of
        NoMouseButton ->
            case List.head model.drawing of
                Just _ ->
                    case List.head model.snappedDrawingPoints of
                        Just _ ->
                            False

                        Nothing ->
                            (model.drawing |> List.map positionFromCoordinate |> tapPositionRange) < 0.4

                Nothing ->
                    False

        _ ->
            False


tapPositionRange : List Position -> Float
tapPositionRange positions =
    let
        rowRange =
            positions |> positionsAxisRange (\p -> p.row)

        columnRange =
            positions |> positionsAxisRange (\p -> p.column)
    in
    max rowRange columnRange


positionsAxisRange : (Position -> Float) -> List Position -> Float
positionsAxisRange axis positions =
    let
        axisValues =
            positions |> List.map axis

        maybeAxisMin =
            List.minimum axisValues

        maybeAxisMax =
            List.maximum axisValues
    in
    case maybeAxisMin of
        Just axisMin ->
            case maybeAxisMax of
                Just axisMax ->
                    axisMax - axisMin

                Nothing ->
                    0

        Nothing ->
            0


withinBoard : Position -> Bool
withinBoard position =
    numberBetween -0.5 (gridSize.rowCount - 0.5) position.row
        && numberBetween -0.5 (gridSize.columnCount - 0.5) position.column


completeMazeDrawing : Model -> Model
completeMazeDrawing model =
    let
        activeMaze =
            model.mazes |> Tuple.first
    in
    case activeMaze.stage of
        DrawingStage ->
            { model
                | mazes =
                    model.mazes
                        |> Tuple.mapFirst
                            (\maze ->
                                { maze
                                    | walls = activeMaze.walls |> hideAllWalls
                                    , stage = PlayingStage
                                    , pathTravelled = [ activeMaze.position ]
                                }
                            )
                , eventsQueuedForSubmission =
                    List.concat
                        [ model.eventsQueuedForSubmission
                        , [ { event =
                                MazeDrawn
                                    { walls = activeMaze.walls |> backendWallsFromWalls
                                    , golds = activeMaze.golds
                                    }
                            , version = model.gameStateVersion + List.length model.eventsQueuedForSubmission + 1
                            }
                          ]
                        ]
            }
                |> startSwitchingMazesIfOtherMazeNotWon

        PlayingStage ->
            model

        FirstWinStage ->
            model


endGameIfWon : Model -> Model
endGameIfWon model =
    let
        winMessage =
            "You win! :D"

        cameSecondMessage =
            "You finished!"

        message =
            case inactiveMaze.stage of
                DrawingStage ->
                    "Error"

                PlayingStage ->
                    winMessage

                FirstWinStage ->
                    cameSecondMessage

        winPopup =
            Just { messageLines = [ message ] }

        activeMaze =
            model.mazes |> Tuple.first

        inactiveMaze =
            model.mazes |> Tuple.second
    in
    case (model.mazes |> Tuple.first).stage of
        DrawingStage ->
            model

        PlayingStage ->
            if
                (activeMaze.currentMove == Nothing)
                    && (activeMaze.golds |> List.any (\gold -> gold == activeMaze.position))
            then
                { model
                    | mazes =
                        model.mazes
                            |> Tuple.mapFirst
                                (\maze ->
                                    { maze
                                        | walls = activeMaze.walls |> revealAllWalls
                                        , stage = FirstWinStage
                                    }
                                )
                    , popup = winPopup
                }

            else
                model

        FirstWinStage ->
            model



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        animationActive =
            ((model.mazes |> Tuple.first).currentMove /= Nothing)
                || wallsAreAnimating (model.mazes |> Tuple.first).walls
                || (model.switchingMaze /= NotSwitchingMaze)
                || not (List.isEmpty model.queuedEventsForApplication)

        animationSubscription =
            if animationActive then
                [ onAnimationFrameDelta Tick ]

            else
                []
    in
    Sub.batch
        (List.concat
            [ [ Keyboard.downs KeyDown ]
            , animationSubscription
            ]
        )


wallsAreAnimating : List Wall -> Bool
wallsAreAnimating walls =
    List.any wallIsAnimating walls


wallIsAnimating : Wall -> Bool
wallIsAnimating wall =
    (wall.hidden == True && not (wall.opacity == 0))
        || (wall.hidden == False && not (wall.opacity == 1))


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

                    3 ->
                        Json.Decode.succeed MiddleMouseButton

                    _ ->
                        Json.Decode.succeed NoMouseButton
            )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "PathFinder"
    , body =
        [ div
            [ css
                [ fontFamily
                , fontSize
                , Css.width (Css.pct 100)
                , Css.height (Css.pct 100)
                , Css.property "perspective" "1000px"
                ]
            ]
            [ viewBackground
            , lazy viewBoard model
            , lazy viewButtons (model.mazes |> Tuple.first).stage
            , viewGithubLink
            ]
            |> toUnstyled
        ]
    }


fontFamily =
    Css.fontFamilies [ "Roboto", "Arial", "Helvetica", "sans-serif" ]


fontSize =
    Css.fontSize (px 30)


positionFromCoordinate : Coordinate -> Position
positionFromCoordinate coords =
    { column = coords.x |> columnFromX
    , row = coords.y |> rowFromY
    }


coordsFromPosition : Position -> Coordinate
coordsFromPosition position =
    { x = position.column |> xFromColumn
    , y = position.row |> yFromRow
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
    let
        mouseEvents =
            [ updateMouseOn "pointerdown"
            , updateMouseOn "pointerup"
            , updateMouseOn "pointermove"
            ]

        viewDrawingStage =
            case activeMaze.stage of
                DrawingStage ->
                    [ lazy viewDrawing model.drawing
                    , lazy viewSnappedDrawingPoints model.snappedDrawingPoints
                    ]

                PlayingStage ->
                    []

                FirstWinStage ->
                    []

        flipDegrees =
            case model.switchingMaze of
                SwitchingMaze progressFraction ->
                    if progressFraction >= 0 then
                        180 |> Debug.log "flipDegrees"

                    else
                        0 |> Debug.log "flipDegrees"

                NotSwitchingMaze ->
                    0

        activeMaze =
            mazes |> Tuple.first

        frontMaze =
            mazes |> Tuple.first

        backMaze =
            mazes |> Tuple.second

        mazes =
            model.mazes

        flipAnimateCss =
            case model.switchingMaze of
                SwitchingMaze progressFraction ->
                    [ Css.Transitions.transition [ Css.Transitions.transform (settings.mazeSwitching.animationDurationSeconds * 1000) ] ]

                NotSwitchingMaze ->
                    []
    in
    div
        (List.concat
            [ [ css
                    (List.concat
                        [ [ width (px settings.boardWidth)
                          , height (px settings.boardHeight)
                          , Css.touchAction Css.none
                          , Css.property "zoom" ((settings.boardZoom * 100 |> String.fromFloat) ++ "%")
                          , Css.transformStyle Css.preserve3d
                          , Css.transforms [ Css.rotateY (Css.deg flipDegrees) ]

                          --, Css.border3 (px 1) Css.solid (hex "#f00")
                          ]
                        , flipAnimateCss
                        ]
                    )
              ]
            , mouseEvents
            ]
        )
        (List.concat
            [ [ div [ css [ Css.property "backface-visibility" "hidden" ] ] (viewGrid frontMaze)
              , div [ css [ Css.transforms [ Css.rotateY (Css.deg 180) ] ] ] (viewGrid backMaze)
              ]
            , viewDrawingStage
            , [ viewPopup model.popup ]
            ]
        )


viewGrid maze =
    [ viewBoardCells
    , viewPathTravelled maze.pathTravelled
    , lazy viewGolds maze.golds
    , lazy viewPlayer maze.position
    , lazy viewWalls maze.walls
    ]


viewBackground =
    div
        [ css
            [ width (pct 100)
            , height (px 1400)
            , Css.position absolute
            , Css.top (px 0)
            , Css.left (px 0)
            , Css.zIndex (Css.int -9999)
            , Css.backgroundImage
                (Css.linearGradient2
                    (Css.deg 37)
                    (Css.stop2 (hex "287fc4") (Css.pct 0))
                    (Css.stop2 (hex "6dff66") (Css.pct 100))
                    []
                )
            , Css.animationName
                (Css.Animations.keyframes
                    [ ( 0, [ Css.Animations.custom "filter" "hue-rotate(0)" ] )
                    , ( 100, [ Css.Animations.custom "filter" "hue-rotate(360deg)" ] )
                    ]
                )
            , Css.animationDuration (Css.sec 40)
            , Css.property "animation-iteration-count" "infinite"
            ]
        , draggable "false"
        , onContextMenuPreventDefault (Tick 0)
        ]
        []


viewBoardCells =
    div
        [ css
            [ Css.position absolute
            , width (px (gridSize.cellWidth * gridSize.columnCount))
            , height (px (gridSize.cellHeight * gridSize.rowCount))
            , left (px gridBorder.left)
            , top (px gridBorder.top)
            , Css.boxShadow4 (px 0) (px 0) (px 10) (hex "#000")
            ]
        ]
        (List.concatMap
            (\column ->
                List.map
                    (\row ->
                        viewBoardCell { row = row |> toFloat, column = column |> toFloat }
                    )
                    (List.range 0 (gridSize.rowCount - 1))
            )
            (List.range 0 (gridSize.columnCount - 1))
        )


viewBoardCell position =
    img
        [ src "/assets/images/grid_cell_hd.png"
        , css
            [ Css.position absolute
            , width (px gridSize.cellWidth)
            , height (px gridSize.cellHeight)
            , left (px (gridSize.cellWidth * position.column))
            , top (px (gridSize.cellHeight * position.row))
            ]
        , draggable "false"
        , onContextMenuPreventDefault (Tick 0)
        ]
        []


viewPathTravelled : PathTravelled -> Html.Styled.Html Msg
viewPathTravelled pathTravelled =
    div []
        (case List.head pathTravelled of
            Just firstPosition ->
                (firstPosition |> viewPathTravelledSquare)
                    :: (pathTravelled
                            |> listMapConsecutively viewPathTravelledSegment
                       )

            Nothing ->
                []
        )


viewPathTravelledSegment : Position -> Position -> Html.Styled.Html Msg
viewPathTravelledSegment pointA pointB =
    let
        midpoint =
            calculateMidpoint pointA pointB
    in
    div []
        [ viewPathTravelledSquare midpoint
        , viewPathTravelledSquare pointB
        ]


viewPathTravelledSquare : Position -> Html.Styled.Html Msg
viewPathTravelledSquare position =
    let
        gapTop =
            4

        gapLeft =
            4
    in
    div
        [ css
            [ Css.position absolute
            , width (px (gridSize.cellWidth - gapLeft))
            , height (px (gridSize.cellHeight - gapTop))
            , left (px (gridSize.cellWidth * position.column + gridBorder.left + gapLeft / 2))
            , top (px (gridSize.cellHeight * position.row + gridBorder.top + gapTop / 2))
            , Css.backgroundColor (hex "#79be7e")
            ]
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


viewPlayer position =
    img
        [ src "/assets/images/player_hd.png"
        , css
            [ Css.position absolute
            , width (px 11)
            , height (px 31)
            , left (px ((position.column |> xFromColumn) - (11 / 2) |> roundFloat))
            , top (px ((position.row |> yFromRow) - 31 / 2 + 1 |> roundFloat))
            ]
        , draggable "false"
        , onContextMenuPreventDefault (Tick 0)
        ]
        []


viewGolds golds =
    div [] (golds |> List.map viewGold)


viewGold gold =
    img
        [ src "/assets/images/golden_hd.png"
        , css
            [ Css.position absolute
            , width (px 17)
            , height (px 17)
            , left (px ((gold.column |> xFromColumn) - (17 / 2) |> roundFloat))
            , top (px ((gold.row |> yFromRow) - 17 / 2 |> roundFloat))
            ]
        , draggable "false"
        , onContextMenuPreventDefault (Tick 0)
        ]
        []


viewWalls walls =
    div [] (walls |> List.map viewWall)


viewWall wall =
    if wall.orientation == Vertical then
        img
            [ src "/assets/images/wall_h_hd.png"
            , css
                [ Css.position absolute
                , height (px 8)
                , width (px 32)
                , left (px ((wall.column |> xFromColumn) - 16 |> roundFloat))
                , top (px ((wall.row |> yFromRow) - 4 |> roundFloat))
                , opacity (Css.num wall.opacity)
                , transform (rotate (Css.deg 90))
                ]
            , draggable "false"
            , onContextMenuPreventDefault (Tick 0)
            ]
            []

    else
        img
            [ src "/assets/images/wall_h_hd.png"
            , css
                [ Css.position absolute
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


viewDrawing drawing =
    let
        drawnSegments =
            drawing |> listMapConsecutively viewKeyedDrawingSegment
    in
    Keyed.node "div" [] drawnSegments


viewKeyedDrawingSegment coordA coordB =
    ( coordToString coordA ++ coordToString coordB, lazy2 viewDrawingSegment coordA coordB )


coordToString coord =
    String.fromFloat coord.x ++ "," ++ String.fromFloat coord.y


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
            [ Css.position absolute
            , left (px (l - 2))
            , top (px (t - 2))
            , width (px h)
            , height (px 0)
            , backgroundColor (hex "#000")
            , transform (rotate (rad angle))
            , border3 (px 2) Css.solid (hex "#000")
            , Css.borderRadius (px 4)
            ]
        ]
        []


viewSnappedDrawingPoints snappedDrawingPoints =
    div [] (snappedDrawingPoints |> List.map viewSnappedDrawingPoint)


viewSnappedDrawingPoint snappedDrawingPoint =
    div
        [ css
            [ Css.position absolute
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


viewPopup maybePopup =
    case maybePopup of
        Just popup ->
            div
                [ css
                    [ Css.position absolute
                    , width (px 300)
                    , height (px 160)
                    , left (px 168)
                    , top (px 150)
                    , backgroundColor (hex "#01b5b5")
                    , Css.border3 (px 2) Css.solid (hex "#000")
                    , Css.textAlign Css.center
                    , Css.displayFlex
                    , Css.flexDirection Css.column
                    , Css.justifyContent Css.center
                    , Css.boxShadow4 (px 0) (px 0) (px 39) (hex "#000")
                    , Css.opacity (Css.num 0.8)
                    ]
                ]
                [ div [] (popup.messageLines |> List.map (\messageLine -> div [] [ text messageLine ]))
                , viewPopupDismissButton "Close"
                ]

        Nothing ->
            div [] []


viewButtons stage =
    case stage of
        DrawingStage ->
            div []
                [ div [ css [ Css.displayFlex ] ]
                    [ viewDoneButton
                    ]
                , div [] [ text "Draw walls with your mouse/finger." ]
                , div [] [ text "Click/tap to place a gold." ]
                , div [] [ text "Hold the right mouse button to remove." ]
                , div [] [ text "Press Done/Enter when finished drawing." ]
                ]

        PlayingStage ->
            div []
                [ div [ css [ Css.displayFlex ] ]
                    [ viewArrowButton MoveLeft "<"
                    , viewArrowButton MoveRight ">"
                    , viewArrowButton MoveUp "^"
                    , viewArrowButton MoveDown "v"
                    ]
                , div [] [ text "Swipe to move, or use the buttons / WASD / arrow keys." ]
                ]

        FirstWinStage ->
            div [] []


viewPopupDismissButton : String -> Html.Styled.Html Msg
viewPopupDismissButton buttonText =
    button
        [ css
            [ width (px 120)
            , height (px 50)
            , Css.margin2 (px 0) Css.auto
            , Css.marginTop (px 15)
            , Css.fontSize (px 30)
            , fontFamily
            ]
        , onClick DismissPopup
        ]
        [ text buttonText ]


viewArrowButton moveDirection buttonText =
    button
        [ css
            [ width (px 100)
            , height (px 100)
            , Css.fontSize (px 80)
            , Css.marginRight (px 50)
            , fontFamily
            ]
        , onClick (MoveButtonPressed moveDirection)
        ]
        [ text buttonText ]


viewDoneButton =
    button
        [ css
            [ width (px 160)
            , height (px 100)
            , Css.fontSize (px 50)
            , fontFamily
            ]
        , onClick DoneButtonPressed
        ]
        [ text "Done" ]


viewGithubLink =
    div [] [ a [ href "https://github.com/ZimbiX/pathfinder-elm" ] [ text "GitHub" ] ]
