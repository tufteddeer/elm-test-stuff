module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main exposing (maxLengthString)
import Test exposing (..)


suite : Test
suite =
    test "maxStringLength"
        (\_ ->
            let
                inputs =
                    [ "short", "verylongstring", "10charstri" ]

                expectations =
                    [ "short", "verylon...", "10charstri" ]
            in
            inputs
                |> List.map (maxLengthString 10)
                |> Expect.equal expectations
        )
