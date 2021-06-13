module SampleSpec exposing (main)

import Main as App
import Runner
import Spec exposing (..)
import Spec.Claim as Claim
import Spec.Http
import Spec.Http.Route
import Spec.Http.Stub
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


noEventsHttpStub : Spec.Http.Stub.HttpResponseStub
noEventsHttpStub =
    Spec.Http.Stub.for (Spec.Http.Route.get "https://www.zimbico.net/pathfinder-elm-backend/pathfinder-elm-backend.php?id=e9354ce5-8323-4ef6-bbac-994db21e3fc4&after=0")
        |> Spec.Http.Stub.withStatus 200
        |> Spec.Http.Stub.withBody (Spec.Http.Stub.withText "[]")


app : List Spec.Http.Stub.HttpResponseStub -> Setup.Setup App.Model App.Msg
app httpStubs =
    Setup.initForApplication (App.init testFlags)
        |> Setup.withUpdate App.update
        |> Setup.withDocument App.view
        |> Setup.forNavigation
            { onUrlChange = App.UrlChanged
            , onUrlRequest = App.LinkClicked
            }
        |> Spec.Http.Stub.serve httpStubs


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
                |> it "records the name"
                    (Observer.observeModel (\model -> model.mazes.active.creatorName)
                        |> expect
                            (Claim.isEqual Debug.toString (App.ProvidedCreatorName "Fred"))
                    )
            )
        ]


main =
    Runner.browserProgram
        [ loadingScreenSpec
        ]
