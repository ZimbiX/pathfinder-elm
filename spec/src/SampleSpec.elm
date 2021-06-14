module SampleSpec exposing (main)

import Json.Encode
import Main as App
import Runner
import Spec exposing (..)
import Spec.Claim as Claim
import Spec.Http
import Spec.Http.Route
import Spec.Http.Stub exposing (HttpResponseStub)
import Spec.Markup as Markup
import Spec.Markup.Event as Event
import Spec.Markup.Selector exposing (..)
import Spec.Observer as Observer
import Spec.Setup as Setup


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
    { x = App.gridBorder.left + App.settings.boardZoom * App.gridSize.cellWidth * (gridPosition.column + 1)
    , y = App.gridBorder.top + App.settings.boardZoom * App.gridSize.cellHeight * (gridPosition.row + 1)
    }


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
                    , Event.trigger "pointerdown" <|
                        Json.Encode.object
                            [ ( "pageX", Json.Encode.float tileCoordinateGlobal.x )
                            , ( "pageY", Json.Encode.float tileCoordinateGlobal.y )
                            , ( "buttons", Json.Encode.int 1 )
                            ]
                    , Event.trigger "pointerup" <|
                        Json.Encode.object
                            [ ( "pageX", Json.Encode.float tileCoordinateGlobal.x )
                            , ( "pageY", Json.Encode.float tileCoordinateGlobal.y )
                            , ( "buttons", Json.Encode.int 0 )
                            ]
                    ]
                |> it "places a gold there"
                    (Observer.observeModel (\model -> model.mazes.active.golds)
                        |> expect (Claim.isEqual Debug.toString [ tilePosition ])
                    )
            )
        ]


main =
    Runner.browserProgram
        [ loadingScreenSpec
        ]
