module CrockfordTests exposing (suite)

import Crockford exposing (decode, encode)
import Expect exposing (Expectation)
import Fuzz
import Random
import Test exposing (..)


suite : Test
suite =
    describe "Crockford"
        [ encodingTests
        , decodingTests
        ]


encodingTests =
    test "encoding" <|
        \_ ->
            Expect.equal "16J" (encode 1234)


decodingTests =
    describe "decoding"
        [ fuzz nonNegativeInt "round trip" <|
            \n -> Expect.equal n (decode (encode n))
        , test "`I` is treated as `1`" <|
            \_ ->
                Expect.equal 1 (decode "I")
        ]


nonNegativeInt =
    Fuzz.intRange 0 Random.maxInt
