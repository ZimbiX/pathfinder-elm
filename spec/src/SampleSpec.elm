module SampleSpec exposing (main)

import Main as App
import Runner
import Spec exposing (..)
import Spec.Claim as Claim
import Spec.Markup as Markup
import Spec.Markup.Event as Event
import Spec.Markup.Selector exposing (..)
import Spec.Setup as Setup


randomSeed =
    ( 0, [ 0, 0, 0, 0 ] )


testFlags : App.Flags
testFlags =
    randomSeed


loadingScreenSpec : Spec App.Model App.Msg
loadingScreenSpec =
    describe "an html program"
        [ scenario "a click event"
            (given
                (Setup.initForApplication (App.init testFlags)
                    |> Setup.withUpdate App.update
                    |> Setup.withDocument App.view
                    |> Setup.forNavigation
                        { onUrlChange = App.UrlChanged
                        , onUrlRequest = App.LinkClicked
                        }
                )
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
        ]


main =
    Runner.browserProgram
        [ loadingScreenSpec
        ]
