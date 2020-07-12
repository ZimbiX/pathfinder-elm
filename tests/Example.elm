module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main
import Test exposing (..)


suite : Test
suite =
    describe "listMapConsecutively"
        [ test "with a few values, it maps them, returning one fewer elements than the input"
            (\_ ->
                [ "1", "2", "3", "4" ]
                    |> Main.listMapConsecutively (\a b -> a ++ "-" ++ b)
                    |> Expect.equal [ "1-2", "2-3", "3-4" ]
            )
        , test "with one element, it returns an empty list"
            (\_ ->
                [ "1" ]
                    |> Main.listMapConsecutively (\a b -> a ++ "-" ++ b)
                    |> Expect.equal []
            )
        , test "with an empty list, it returns an empty list"
            (\_ ->
                []
                    |> Main.listMapConsecutively (\a b -> a ++ "-" ++ b)
                    |> Expect.equal []
            )
        ]
