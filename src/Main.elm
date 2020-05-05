module Main exposing (Model, Msg(..), init, main, update, view)

import Basics.Extra exposing (fractionalModBy)
import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Browser.Navigation as Nav
import Css exposing (absolute, backgroundColor, border3, height, hex, left, opacity, pct, px, rad, rotate, solid, top, transform, width)
import Css.Animations
import Css.Transitions
import Debug
import Html exposing (Html)
import Html.Events.Extra
import Html.Styled exposing (a, button, div, img, text, toUnstyled)
import Html.Styled.Attributes exposing (class, css, draggable, href, src)
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
import Prng.Uuid as Uuid
import Process
import Random.Pcg.Extended exposing (Seed, initialSeed, step)
import Task
import Url
import Url.Parser
import Url.Parser.Query



-- SETTINGS


settings =
    { playerMoveSpeed = 0.007
    , wallOpacitySpeed = 0.008
    , boardZoom = 2
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
    { top = 32, left = 32 }



-- MAIN


main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


type alias Model =
    { mazes : Mazes
    , mouse : Mouse
    , drawing : Drawing
    , snappedDrawingPoints : SnappedDrawingPoints
    , popup : Maybe Popup
    , switchingMaze : SwitchingMazeState
    , gameStateVersion : Int
    , queuedEventsForApplication : List VersionedBackendEvent
    , eventsQueuedForSubmission : List VersionedBackendEvent
    , navKey : Nav.Key
    , url : Url.Url
    , currentSeed : Seed
    , gameId : String
    }


type alias Mazes =
    { active : Maze
    , inactive : Maze
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
    | WaitingForOtherMazeToBeDrawnStage
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


init : ( Int, List Int ) -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init ( seed, seedExtension ) url navKey =
    { mazes =
        { active = initialMaze
        , inactive = initialMaze
        }
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
    , navKey = navKey
    , url = url
    , currentSeed = initialSeed seed seedExtension
    , gameId = ""
    }
        |> assignGameId url
        |> startEventPolling


initialMaze =
    { position = { column = 0, row = 0 }
    , currentMove = Nothing
    , walls = []
    , golds = []
    , stage = DrawingStage
    , pathTravelled = []
    }


assignGameId : Url.Url -> Model -> ( Model, Cmd Msg )
assignGameId url model =
    case readGameIdFromUrl url of
        Just gameIdFromUrl ->
            ( { model | gameId = gameIdFromUrl |> Debug.log "Game ID" }
            , Cmd.none
            )

        Nothing ->
            model
                |> generateGameId
                |> (\newModel ->
                        ( newModel
                        , Nav.pushUrl newModel.navKey ("#gameId=" ++ newModel.gameId)
                        )
                   )


readGameIdFromUrl : Url.Url -> Maybe String
readGameIdFromUrl url =
    url
        |> fragmentUrlToQueryUrl
        |> Url.Parser.parse (Url.Parser.query (Url.Parser.Query.string "gameId"))
        |> Maybe.withDefault Nothing


fragmentUrlToQueryUrl : Url.Url -> Url.Url
fragmentUrlToQueryUrl url =
    { url
        | path = ""
        , query = url.fragment
        , fragment = Nothing
    }


generateGameId : Model -> Model
generateGameId model =
    let
        ( newUuid, newSeed ) =
            step Uuid.generator model.currentSeed
    in
    { model
        | gameId = newUuid |> Uuid.toString |> Debug.log "Game ID"
        , currentSeed = newSeed
    }


startEventPolling : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
startEventPolling ( model, cmd ) =
    ( model, cmd )
        |> Tuple.mapSecond (\c -> Cmd.batch [ c, requestNewEvents model.gameId 0 ])



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
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url


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
            model
                |> updateMouseFromUnscaled unscaledMouse
                |> updateDrawing
                |> mouseProcessorForStageInteractions model.mazes.active.stage
                |> clearDrawingIfFinished
                |> submitQueuedEvents

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
            ( model, requestNewEvents model.gameId model.gameStateVersion )

        GotEventsFromBackend result ->
            case result of
                Ok eventsResponse ->
                    case eventsResponse |> Json.Decode.decodeString eventsDecoder of
                        Ok events ->
                            model
                                |> queueNewEventsForApplication events

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

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, url |> Url.toString |> Nav.pushUrl model.navKey )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url |> Debug.log "New URL" }
            , Cmd.none
            )


updateActiveMaze : (Maze -> Maze) -> Model -> Model
updateActiveMaze mazeUpdater model =
    { model
        | mazes =
            model.mazes
                |> (\mazes ->
                        { mazes
                            | active =
                                mazes.active
                                    |> mazeUpdater
                        }
                   )
    }


updateInactiveMaze : (Maze -> Maze) -> Model -> Model
updateInactiveMaze mazeUpdater model =
    { model
        | mazes =
            model.mazes
                |> (\mazes ->
                        { mazes
                            | inactive =
                                mazes.inactive
                                    |> mazeUpdater
                        }
                   )
    }


applyNextEventFromServerIfReady : Model -> Model
applyNextEventFromServerIfReady model =
    case List.Extra.uncons model.queuedEventsForApplication of
        Just ( firstEvent, laterEvents ) ->
            let
                moveIfReady direction =
                    if playerCanStartMove model then
                        model
                            |> startPlayerMove direction
                            |> (\newModel -> { newModel | queuedEventsForApplication = laterEvents })

                    else
                        model

                --expectedNextVersion =
                --    model.gameStateVersion + 1
            in
            --if firstEvent.version == expectedNextVersion then
            case firstEvent.event of
                MazeDrawn mazeBackend ->
                    model
                        |> updateActiveMaze
                            (\maze ->
                                { maze
                                    | golds = mazeBackend.golds
                                    , walls = mazeBackend.walls |> wallsFromBackendWalls
                                }
                            )
                        |> completeMazeDrawing
                        |> swapMazes
                        |> (\newModel -> { newModel | queuedEventsForApplication = laterEvents })

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
        --else
        --    Debug.log ("Warning: Unexpected event version: " ++ String.fromInt firstEvent.version ++ " /= " ++ String.fromInt expectedNextVersion) ""
        --        |> (\_ -> model)
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
    in
    case List.Extra.last events of
        Just latestEvent ->
            ( { model
                | gameStateVersion =
                    latestEvent.version
                        |> Debug.log "updated gameStateVersion to reflect version of latest event in queuedEventsForApplication"
                , queuedEventsForApplication = queuedEventsForApplication
              }
              --, Cmd.none
            , Process.sleep 100 |> Task.perform (\_ -> RequestNewEventsFromBackend)
            )

        Nothing ->
            ( model
              --, Cmd.none
            , Process.sleep 1000 |> Task.perform (\_ -> RequestNewEventsFromBackend)
            )


requestNewEvents : String -> Int -> Cmd Msg
requestNewEvents gameId afterVersion =
    Http.get
        { url = "https://www.zimbico.net/pathfinder-elm-backend/pathfinder-elm-backend.php?id=" ++ gameId ++ "&after=" ++ String.fromInt afterVersion
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


submitEvent : String -> VersionedBackendEvent -> Cmd Msg
submitEvent gameId versionedEvent =
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
            [ ( "id", gameId )
            , ( "version", String.fromInt versionedEvent.version )
            , ( "event", Json.Encode.encode 0 encodedEvent )
            ]
    in
    Http.post
        { url = "https://www.zimbico.net/pathfinder-elm-backend/pathfinder-elm-backend.php"
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
            List.map (submitEvent model.gameId) versionedEvents

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

        WaitingForOtherMazeToBeDrawnStage ->
            \x -> x

        PlayingStage ->
            tryStartPlayerMoveFromSwipe

        FirstWinStage ->
            \x -> x


updateMouseFromUnscaled : Mouse -> Model -> Model
updateMouseFromUnscaled unscaledMouse model =
    { model | mouse = { unscaledMouse | position = unscaledMouse.position |> zoomCoordinate |> toGridCoordinateFromGlobal } }


toGridCoordinateFromGlobal : Coordinate -> Coordinate
toGridCoordinateFromGlobal coord =
    { x = coord.x - gridBorder.left
    , y = coord.y - gridBorder.top
    }


zoomCoordinate : Coordinate -> Coordinate
zoomCoordinate coord =
    { x = coord.x / settings.boardZoom
    , y = coord.y / settings.boardZoom
    }


updatePlayerPosition : Float -> Model -> Model
updatePlayerPosition deltaTime model =
    let
        activeMaze =
            model.mazes.active

        position =
            activeMaze.position
    in
    case activeMaze.currentMove of
        Just currentMove ->
            let
                moveDistance =
                    settings.playerMoveSpeed * deltaTime

                column =
                    case currentMove.direction of
                        MoveRight ->
                            position.column + moveDistance

                        MoveLeft ->
                            position.column - moveDistance

                        _ ->
                            position.column

                row =
                    case currentMove.direction of
                        MoveDown ->
                            position.row + moveDistance

                        MoveUp ->
                            position.row - moveDistance

                        _ ->
                            position.row

                newPosition =
                    Position column row
            in
            model |> updateActiveMaze (\maze -> { maze | position = newPosition })

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
    case model.mazes.active.currentMove of
        Just _ ->
            False

        Nothing ->
            case model.mazes.active.stage of
                PlayingStage ->
                    case model.switchingMaze of
                        SwitchingMaze _ ->
                            False

                        NotSwitchingMaze ->
                            True

                WaitingForOtherMazeToBeDrawnStage ->
                    False

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
    case model.mazes.active.currentMove of
        Just currentMove ->
            if playerWithinMoveBounds model currentMove then
                model

            else if currentMove.reversing then
                model
                    |> updateActiveMaze
                        (\maze ->
                            { maze
                                | position = currentMove.origin
                                , currentMove = Nothing
                            }
                        )
                    |> startSwitchingMazesIfOtherMazeNotWon

            else
                let
                    pathTravelled =
                        List.concat [ [ currentMove.target ], model.mazes.active.pathTravelled ]
                in
                model
                    |> updateActiveMaze
                        (\maze ->
                            { maze
                                | position = currentMove.target
                                , currentMove = Nothing
                                , pathTravelled = pathTravelled
                            }
                        )

        Nothing ->
            model


returnToOriginIfPathUnclear : Model -> Model
returnToOriginIfPathUnclear model =
    if pathAheadClear model then
        model

    else
        case model.mazes.active.currentMove of
            Just currentMove ->
                if currentMove.reversing then
                    model

                else
                    let
                        returnMove =
                            Just { currentMove | reversing = True, direction = oppositeDirection currentMove.direction }
                    in
                    model
                        |> updateActiveMaze (\maze -> { maze | currentMove = returnMove })
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
    case model.mazes.active.currentMove of
        Just currentMove ->
            let
                activeMaze =
                    model.mazes.active

                origin =
                    currentMove.origin

                target =
                    currentMove.target

                position =
                    activeMaze.position
            in
            if not (withinBoard activeMaze.position) then
                False

            else if wallExistsBetweenPoints activeMaze.walls origin target then
                (numberBetween origin.column (origin.column + 0.4) position.column
                    || numberBetween origin.column (origin.column - 0.4) position.column
                )
                    && (numberBetween origin.row (origin.row + 0.4) position.row
                            || numberBetween origin.row (origin.row - 0.4) position.row
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
    case model.mazes.active.currentMove of
        Just currentMove ->
            let
                currentMoveMidpoint =
                    calculateMidpoint currentMove.origin currentMove.target

                walls =
                    model.mazes.active.walls |> List.map (revealWallIfAtPoint currentMoveMidpoint)
            in
            model |> updateActiveMaze (\maze -> { maze | walls = walls })

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
    { model | popup = Nothing }


switchMazeIfMPressed : RawKey -> Model -> Model
switchMazeIfMPressed rawKey model =
    case Keyboard.anyKeyUpper rawKey of
        Just (Character "M") ->
            model |> startSwitchingMazes

        _ ->
            model


startSwitchingMazesIfOtherMazeNotWon : Model -> Model
startSwitchingMazesIfOtherMazeNotWon model =
    case model.mazes.inactive.stage of
        DrawingStage ->
            model |> startSwitchingMazes

        WaitingForOtherMazeToBeDrawnStage ->
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
            { active = model.mazes.inactive
            , inactive = model.mazes.active
            }
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
            numberBetween origin.column target.column position.column

        rowWithinMoveBounds =
            numberBetween origin.row target.row position.row

        origin =
            currentMove.origin

        target =
            currentMove.target

        position =
            model.mazes.active.position
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
            model.mazes.active

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

                isApplyingServerEvent =
                    not (List.isEmpty model.queuedEventsForApplication)

                eventsQueuedForSubmission =
                    if isApplyingServerEvent then
                        []

                    else
                        List.concat [ model.eventsQueuedForSubmission, [ versionedEvent ] ]
            in
            { model | eventsQueuedForSubmission = eventsQueuedForSubmission }
                |> updateActiveMaze (\maze -> { maze | currentMove = newMove })
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
            model.mazes.active

        walls =
            List.map (updateWallOpacity deltaTime) activeMaze.walls
    in
    model |> updateActiveMaze (\maze -> { maze | walls = walls })


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



-- Produce a list of numbers in the range given, interpolating by 1
-- e.g.:
-- rangeFloatByIncrementDirected 1.5 4.5 -> [1.5, 2.5, 3.5, 4.5]
-- rangeFloatByIncrementDirected 4.5 1.5 -> [4.5, 3.5, 2.5, 1.5]


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
                            listOfMaybeWalls |> Maybe.Extra.values

                        walls =
                            List.concat [ newWalls, model.mazes.active.walls ]
                    in
                    model |> updateActiveMaze (\maze -> { maze | walls = walls })

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
                    List.concat [ [ nearestGridCenter ], model.mazes.active.golds ]
            in
            model |> updateActiveMaze (\maze -> { maze | golds = golds })

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
                    List.filter (itemIsNotUnderPointer model.mouse) model.mazes.active.golds
            in
            model |> updateActiveMaze (\maze -> { maze | golds = golds })

        _ ->
            model


deleteWall : Model -> Model
deleteWall model =
    case model.mouse.buttonDown of
        RightMouseButton ->
            let
                walls =
                    List.filter (wallIsNotUnderPointer model.mouse) model.mazes.active.walls
            in
            model |> updateActiveMaze (\maze -> { maze | walls = walls })

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
            model.mazes.active

        complete =
            let
                isApplyingServerEvent =
                    not (List.isEmpty model.queuedEventsForApplication)

                eventsQueuedForSubmission =
                    if isApplyingServerEvent then
                        []

                    else
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
            in
            { model | eventsQueuedForSubmission = eventsQueuedForSubmission }
                |> updateActiveMaze
                    (\maze ->
                        { maze
                            | walls = activeMaze.walls |> hideAllWalls
                            , stage = WaitingForOtherMazeToBeDrawnStage
                            , pathTravelled = [ activeMaze.position ]
                        }
                    )
                |> (\newModel ->
                        case newModel.mazes.inactive.stage of
                            DrawingStage ->
                                newModel |> startSwitchingMazesIfOtherMazeNotWon

                            WaitingForOtherMazeToBeDrawnStage ->
                                newModel
                                    |> updateActiveMaze (\maze -> { maze | stage = PlayingStage })
                                    |> updateInactiveMaze (\maze -> { maze | stage = PlayingStage })

                            PlayingStage ->
                                newModel

                            FirstWinStage ->
                                newModel
                   )
    in
    case activeMaze.stage of
        DrawingStage ->
            complete

        WaitingForOtherMazeToBeDrawnStage ->
            -- Impossible to get to?
            model

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

                WaitingForOtherMazeToBeDrawnStage ->
                    "Error"

                PlayingStage ->
                    winMessage

                FirstWinStage ->
                    cameSecondMessage

        winPopup =
            Just { messageLines = [ message ] }

        activeMaze =
            model.mazes.active

        inactiveMaze =
            model.mazes.inactive
    in
    case model.mazes.active.stage of
        DrawingStage ->
            model

        WaitingForOtherMazeToBeDrawnStage ->
            model

        PlayingStage ->
            if
                (activeMaze.currentMove == Nothing)
                    && (activeMaze.golds |> List.any (\gold -> gold == activeMaze.position))
            then
                { model | popup = winPopup }
                    |> updateActiveMaze
                        (\maze ->
                            { maze
                                | walls = activeMaze.walls |> revealAllWalls
                                , stage = FirstWinStage
                            }
                        )

            else
                model

        FirstWinStage ->
            model



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        animationActive =
            (model.mazes.active.currentMove /= Nothing)
                || wallsAreAnimating model.mazes.active.walls
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
            [ class "view"
            , css
                [ fontFamily
                , fontSize
                , Css.width (Css.pct 100)
                , Css.height (Css.pct 100)
                ]
            ]
            [ viewBackground
            , lazy viewBoard model
            , lazy2 viewButtons model.mazes.active.stage (model.switchingMaze /= NotSwitchingMaze)
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
    gridSize.cellWidth * (0.5 + column)


yFromRow : Float -> Float
yFromRow row =
    gridSize.cellHeight * (0.5 + row)



-- Workings:
-- x = gridBorder.left + gridSize.cellWidth * (0.5 + column) ; minus gridBorder.left
-- x - gridBorder.left = gridSize.cellWidth * (0.5 + column) ; divide by gridSize.cellWidth
-- (x - gridBorder.left) / gridSize.cellWidth = 0.5 + column ; minus 0.5
-- (x - gridBorder.left) / gridSize.cellWidth - 0.5 = column


columnFromX : Float -> Float
columnFromX x =
    x / gridSize.cellWidth - 0.5


rowFromY : Float -> Float
rowFromY y =
    y / gridSize.cellHeight - 0.5


roundFloat =
    round >> toFloat


viewBoard : Model -> Html.Styled.Html Msg
viewBoard model =
    let
        viewDrawingStage =
            case model.mazes.active.stage of
                DrawingStage ->
                    [ lazy viewDrawing model.drawing
                    , lazy viewSnappedDrawingPoints model.snappedDrawingPoints
                    ]

                WaitingForOtherMazeToBeDrawnStage ->
                    []

                PlayingStage ->
                    []

                FirstWinStage ->
                    []

        mouseEvents =
            [ updateMouseOn "pointerdown"
            , updateMouseOn "pointerup"
            , updateMouseOn "pointermove"
            ]
    in
    div
        (List.concat
            [ [ class "viewBoard"
              , css
                    [ Css.property "zoom" ((settings.boardZoom * 100 |> String.fromFloat) ++ "%")
                    , width (px (gridSize.cellWidth * gridSize.columnCount + gridBorder.left * 2))
                    , height (px (gridSize.cellHeight * gridSize.rowCount + gridBorder.top * 2))
                    ]
              ]
            , mouseEvents
            ]
        )
        [ div
            [ css
                [ Css.position absolute
                , left (px gridBorder.left)
                , top (px gridBorder.top)
                ]
            ]
            (List.concat
                [ [ div
                        [ css
                            []
                        ]
                        [ div
                            [ css
                                [ Css.property "perspective" "1000px"
                                , width (px (gridSize.cellWidth * gridSize.columnCount))
                                , height (px (gridSize.cellHeight * gridSize.rowCount))
                                ]
                            ]
                            [ viewMazesWithRotation model ]
                        ]
                  ]
                , viewDrawingStage
                , [ viewPopup model.popup ]
                ]
            )
        ]


viewMazesWithRotation : Model -> Html.Styled.Html Msg
viewMazesWithRotation model =
    let
        flipDegrees =
            case model.switchingMaze of
                SwitchingMaze progressFraction ->
                    if progressFraction >= 0 then
                        180

                    else
                        0

                NotSwitchingMaze ->
                    0

        flipAnimateCss =
            case model.switchingMaze of
                SwitchingMaze progressFraction ->
                    [ Css.Transitions.transition [ Css.Transitions.transform (settings.mazeSwitching.animationDurationSeconds * 1000) ] ]

                NotSwitchingMaze ->
                    []
    in
    div
        [ class "viewMazesWithRotation"
        , css
            (List.concat
                [ [ width (px (gridSize.cellWidth * gridSize.columnCount))
                  , height (px (gridSize.cellHeight * gridSize.rowCount))
                  , Css.touchAction Css.none
                  , Css.transformStyle Css.preserve3d
                  , Css.transforms [ Css.rotateY (Css.deg flipDegrees) ]

                  --, Css.border3 (px 1) Css.solid (hex "#f00")
                  ]
                , flipAnimateCss
                ]
            )
        ]
        [ div
            [ class "activeMaze"
            , css [ Css.property "backface-visibility" "hidden", Css.position Css.relative ]
            ]
            (viewGrid model.mazes.active)
        , div
            [ class "inactiveMaze"
            , css [ Css.transforms [ Css.rotateY (Css.deg 180) ], Css.position Css.relative ]
            ]
            (viewGrid model.mazes.inactive)
        ]


viewGrid : Maze -> List (Html.Styled.Html Msg)
viewGrid maze =
    [ viewBoardCells
    , viewPathTravelled maze.pathTravelled
    , lazy viewGolds maze.golds
    , lazy viewPlayer maze.position
    , lazy viewWalls maze.walls
    ]


viewBackground : Html.Styled.Html Msg
viewBackground =
    div
        [ class "viewBackground"
        , css
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


viewBoardCells : Html.Styled.Html Msg
viewBoardCells =
    div
        [ css
            [ Css.position absolute
            , width (px (gridSize.cellWidth * gridSize.columnCount))
            , height (px (gridSize.cellHeight * gridSize.rowCount))
            , left (px 0)
            , top (px 0)
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
            , left (px (gridSize.cellWidth * position.column + gapLeft / 2))
            , top (px (gridSize.cellHeight * position.row + gapTop / 2))
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
    Keyed.node "div" [ class "viewDrawing" ] drawnSegments


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
    div [ class "viewSnappedDrawingPoints" ] (snappedDrawingPoints |> List.map viewSnappedDrawingPoint)


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
    let
        w =
            300

        h =
            160

        border =
            2
    in
    case maybePopup of
        Just popup ->
            div
                [ class "viewPopup"
                , css
                    [ Css.position absolute
                    , width (px w)
                    , height (px h)
                    , top (px ((gridSize.cellHeight * gridSize.rowCount * 0.95) / 2 - h / 2))
                    , left (px (((gridSize.cellWidth * gridSize.columnCount) / 2 - w / 2) - border))
                    , backgroundColor (hex "#01b5b5")
                    , Css.border3 (px border) Css.solid (hex "#000")
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
            div [ class "viewPopup_nothing" ] []


viewButtons : Stage -> Bool -> Html.Styled.Html Msg
viewButtons stage isSwitchingMaze =
    case stage of
        DrawingStage ->
            div [ class "viewButtons" ]
                [ div [ css [ Css.displayFlex ] ]
                    [ viewDoneButton
                    ]
                , div [] [ text "Draw walls with your mouse/finger." ]
                , div [] [ text "Click/tap to place a gold." ]
                , div [] [ text "Hold the right mouse button to remove." ]
                , div [] [ text "Press Done/Enter when finished drawing." ]
                ]

        WaitingForOtherMazeToBeDrawnStage ->
            if isSwitchingMaze then
                div [ class "viewButtons" ] []

            else
                div [ class "viewButtons" ]
                    [ div [] [ text "Waiting for opponent to finish drawing." ] ]

        PlayingStage ->
            div [ class "viewButtons" ]
                [ div [ css [ Css.displayFlex ] ]
                    [ viewArrowButton MoveLeft "<"
                    , viewArrowButton MoveRight ">"
                    , viewArrowButton MoveUp "^"
                    , viewArrowButton MoveDown "v"
                    ]
                , div [] [ text "Swipe to move, or use the buttons / WASD / arrow keys." ]
                ]

        FirstWinStage ->
            div [ class "viewButtons" ] []


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
    div
        [ class "viewGithubLink" ]
        [ a [ href "https://github.com/ZimbiX/pathfinder-elm" ] [ text "GitHub" ] ]
