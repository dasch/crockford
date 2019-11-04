module CrockfordTests exposing (suite)

import Crockford exposing (..)
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
    describe "encoding"
        [ test "encodes integers" <|
            \_ ->
                Expect.equal "16J" (encode 1234)
        , fuzz nonNegativeInt "encoding with and without checksum" <|
            \n ->
                Expect.equal (decode (encodeWithChecksum n)) (decode (encodeWithChecksum n))
        , test "encoding with checksum" <|
            \_ ->
                Expect.equal "10*" (encodeWithChecksum 32)
        , test "decoding with checksum" <|
            \_ ->
                Expect.equal (Just 42) (decodeWithChecksum "1A5")
        ]


decodingTests =
    describe "decoding"
        [ fuzz nonNegativeInt "round trip" <|
            \n -> Expect.equal n (decode (encode n))
        , test "`I` is treated as `1`" <|
            \_ ->
                Expect.equal 1 (decode "I")
        , test "`L` is treated as `1`" <|
            \_ ->
                Expect.equal 1 (decode "L")
        , test "`O` is treated as `0`" <|
            \_ ->
                Expect.equal 0 (decode "O")
        , test "`-` is ignored" <|
            \_ ->
                Expect.equal (decode "ABCDEF123") (decode "ABC-DEF-123")
        , fuzz nonNegativeInt "decoding is case insensitive" <|
            \n ->
                let
                    encoded =
                        encode n
                in
                Expect.equal (decode (String.toUpper encoded)) (decode (String.toLower encoded))
        ]


nonNegativeInt =
    Fuzz.intRange 0 Random.maxInt
