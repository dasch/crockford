module CrockfordTests exposing (suite)

import Crockford exposing (decode, encode)
import Expect exposing (Expectation)
import Fuzz
import Test exposing (..)


suite : Test
suite =
    describe "Crockford"
        [ encodingTests
        , decodingTests
        ]


encodingTests =
    todo "encoding"


decodingTests =
    describe "decoding"
        [ fuzz Fuzz.int "round trip" <|
            \n -> Expect.equal n (decode (encode n))
        ]
