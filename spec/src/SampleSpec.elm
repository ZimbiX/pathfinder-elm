module SampleSpec exposing (main)

import Json.Encode
import Main as App
import Process
import Runner
import Spec exposing (..)
import Spec.Claim as Claim
import Spec.Command
import Spec.Http
import Spec.Http.Route
import Spec.Http.Stub exposing (HttpResponseStub)
import Spec.Markup as Markup
import Spec.Markup.Event as Event
import Spec.Markup.Selector exposing (..)
import Spec.Observer as Observer
import Spec.Setup as Setup
import Spec.Step
import Spec.Time
import Task


randomSeed =
    ( 0, [ 0, 0, 0, 0 ] )


testFlags : App.Flags
testFlags =
    randomSeed


noEventsHttpStub : HttpResponseStub
noEventsHttpStub =
    Spec.Http.Stub.for (Spec.Http.Route.get "https://www.zimbico.net/pathfinder-elm-backend/pathfinder-elm-backend.php?id=e9354ce5-8323-4ef6-bbac-994db21e3fc4&after=0")
        |> Spec.Http.Stub.withStatus 200
        |> Spec.Http.Stub.withBody (Spec.Http.Stub.withText "[]")


app : List HttpResponseStub -> Setup.Setup App.Model App.Msg
app httpStubs =
    Setup.initForApplication (App.init testFlags)
        |> Setup.withUpdate App.update
        |> Setup.withDocument App.view
        |> Setup.forNavigation
            { onUrlChange = App.UrlChanged
            , onUrlRequest = App.LinkClicked
            }
        |> Spec.Http.Stub.serve httpStubs


tilePosition : App.Position
tilePosition =
    { column = 10, row = 2 }


tileCoordinateGlobal : App.Coordinate
tileCoordinateGlobal =
    gridPositionToGlobalCoordinate tilePosition
        |> Debug.log "tileCoordinateGlobal"


gridPositionToGlobalCoordinate : App.Position -> App.Coordinate
gridPositionToGlobalCoordinate gridPosition =
    { x = columnToGlobalX gridPosition.column
    , y = rowToGlobalY gridPosition.row
    }


columnToGlobalX : Float -> Float
columnToGlobalX column =
    App.gridBorder.left + App.settings.boardZoom * App.gridSize.cellWidth * (column + 1)


rowToGlobalY : Float -> Float
rowToGlobalY row =
    App.gridBorder.top + App.settings.boardZoom * App.gridSize.cellHeight * (row + 1)


loadingScreenSpec : Spec App.Model App.Msg
loadingScreenSpec =
    describe "an html program"
        [ scenario "a click event"
            (given (app [])
                |> it "Shows loading screen"
                    (Markup.observeElement
                        |> Markup.query
                        << by [ class "view" ]
                        |> expect
                            (Claim.isSomethingWhere <|
                                Markup.text <|
                                    Claim.isStringContaining 1 "Reticulating splines..."
                            )
                    )
            )
        , scenario "entering player name"
            (given (app [ noEventsHttpStub ])
                |> when "the player name is entered"
                    [ Markup.target << by [ id "mazeCreatorNameInput" ]
                    , Event.input "Fred"
                    , Markup.target << by [ id "popupDismissButton" ]
                    , Event.click
                    ]
                |> observeThat
                    [ it "records the name"
                        (Observer.observeModel (\model -> model.mazes.active.creatorName)
                            |> expect (Claim.isEqual Debug.toString (App.ProvidedCreatorName "Fred"))
                        )
                    , it "instructs to draw the level"
                        (Markup.observeElement
                            |> Markup.query
                            << by [ class "viewInstructions" ]
                            |> expect
                                (Claim.isSomethingWhere <|
                                    Markup.text <|
                                        Claim.isStringContaining 1 "Fred can now draw the level that Player 2 will later play"
                                )
                        )
                    , it "closes the popup"
                        (Markup.observeElement
                            |> Markup.query
                            << by [ class "viewPopup_none" ]
                            |> expect Claim.isSomething
                        )
                    ]
            )
        , scenario "placing a gold"
            (given (app [ noEventsHttpStub ])
                |> when "the centre of a tile is clicked"
                    [ Markup.target << by [ id "mazeCreatorNameInput" ]
                    , Event.input "Fred"
                    , Markup.target << by [ id "popupDismissButton" ]
                    , Event.click

                    --- TODO: Deduplicate the above
                    , Markup.target << by [ class "activeMaze" ]
                    , pointerdownEvent App.LeftMouseButton tilePosition
                    , pointerupEvent App.NoMouseButton tilePosition
                    ]
                |> it "places a gold there"
                    (Observer.observeModel (\model -> model.mazes.active.golds)
                        |> expect (Claim.isEqual Debug.toString [ tilePosition ])
                    )
            )
        , scenario "drawing walls"
            (given (app [ noEventsHttpStub ])
                |> when "a horizontal line is drawn along tile edges"
                    [ Markup.target << by [ id "mazeCreatorNameInput" ]
                    , Event.input "Fred"
                    , Markup.target << by [ id "popupDismissButton" ]
                    , Event.click

                    --- TODO: Deduplicate the above
                    , Markup.target << by [ class "activeMaze" ]
                    , pointerdownEvent App.LeftMouseButton { column = -0.5, row = -0.5 }
                    , pointermoveEvent App.LeftMouseButton { column = 1.5, row = -0.5 }
                    , pointerupEvent App.NoMouseButton { column = 1.5, row = -0.5 }
                    ]
                |> it "draws walls between the points"
                    (Observer.observeModel (\model -> model.mazes.active.walls)
                        |> expect
                            (Claim.isEqual Debug.toString
                                [ { column = 1, row = -0.5, orientation = App.Horizontal, hidden = False, opacity = 1 }
                                , { column = 0, row = -0.5, orientation = App.Horizontal, hidden = False, opacity = 1 }
                                ]
                            )
                    )
            )
        , scenario "finishing drawing"
            (given (app [ noEventsHttpStub ])
                |> when "a gold is placed, the done button clicked, and the board flip animation waited for"
                    [ Markup.target << by [ id "mazeCreatorNameInput" ]
                    , Event.input "Fred"
                    , Markup.target << by [ id "popupDismissButton" ]
                    , Event.click

                    --- TODO: Deduplicate the above
                    , Markup.target << by [ class "activeMaze" ]
                    , pointerdownEvent App.LeftMouseButton tilePosition
                    , pointerupEvent App.NoMouseButton tilePosition
                    , Markup.target << by [ class "doneButton" ]
                    , Event.click
                    , Spec.Command.send (Process.sleep 0 |> Task.perform (\_ -> App.Tick 4000))
                    ]
                |> observeThat
                    [ it "has finished switching maze"
                        (Observer.observeModel (\model -> model.switchingMaze)
                            |> expect (Claim.isEqual Debug.toString App.NotSwitchingMaze)
                        )
                    , it "switches to the other maze"
                        (Observer.observeModel (\model -> model.mazes.inactive.golds)
                            |> expect (Claim.isEqual Debug.toString [ tilePosition ])
                        )
                    ]
            )
        ]


pointerdownEvent : App.MouseButton -> App.Position -> Spec.Step.Step model msg
pointerdownEvent =
    pointerEvent "pointerdown"


pointerupEvent : App.MouseButton -> App.Position -> Spec.Step.Step model msg
pointerupEvent =
    pointerEvent "pointerup"


pointermoveEvent : App.MouseButton -> App.Position -> Spec.Step.Step model msg
pointermoveEvent =
    pointerEvent "pointermove"


pointerEvent : String -> App.MouseButton -> App.Position -> Spec.Step.Step model msg
pointerEvent eventName mouseButton position =
    Event.trigger eventName <|
        Json.Encode.object
            [ ( "pageX", Json.Encode.float (columnToGlobalX position.column) )
            , ( "pageY", Json.Encode.float (rowToGlobalY position.row) )
            , ( "buttons", Json.Encode.int (mouseButtonNameToInt mouseButton) )
            ]


mouseButtonNameToInt : App.MouseButton -> Int
mouseButtonNameToInt mouseButton =
    case mouseButton of
        App.LeftMouseButton ->
            1

        App.RightMouseButton ->
            2

        App.MiddleMouseButton ->
            3

        App.NoMouseButton ->
            0


main : Program Flags (Model App.Model App.Msg) (Msg App.Msg)
main =
    Runner.browserProgram
        [ loadingScreenSpec
        ]
