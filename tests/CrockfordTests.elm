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
        , checksumTests
        ]


encodingTests =
    describe "encoding"
        [ test "encodes integers" <|
            \_ ->
                Expect.equal (Ok "16J") (encode 1234)
        , test "fails on negative integers" <|
            \_ ->
                Expect.equal (Err NegativeNumberError) (encode -1)
        , fuzz nonNegativeInt "round trip" <|
            \n ->
                encode n
                    |> Result.andThen decode
                    |> Expect.equal (Ok n)
        , test "`I` is treated as `1`" <|
            \_ ->
                Expect.equal (Ok 1) (decode "I")
        , test "`L` is treated as `1`" <|
            \_ ->
                Expect.equal (Ok 1) (decode "L")
        , test "`O` is treated as `0`" <|
            \_ ->
                Expect.equal (Ok 0) (decode "O")
        , test "`-` is ignored" <|
            \_ ->
                Expect.equal (decode "ABCDEF123") (decode "ABC-DEF-123")
        , test "invalid symbols cause failure" <|
            \_ ->
                decode "123#"
                    |> Expect.equal (Err (InvalidCharacter '#'))
        , fuzz nonNegativeInt "decoding is case insensitive" <|
            \n ->
                let
                    encoded =
                        Result.withDefault "" (encode n)
                in
                Expect.equal (decode (String.toUpper encoded)) (decode (String.toLower encoded))
        ]


checksumTests =
    describe "checksums"
        [ test "encoding with checksum" <|
            \_ ->
                encodeWithChecksum 32
                    |> Expect.equal (Ok "10*")
        , test "decoding with checksum" <|
            \_ ->
                decodeWithChecksum "1A5"
                    |> Expect.equal (Ok 42)
        , test "failing on invalid checksum symbol" <|
            \_ ->
                decodeWithChecksum "1A@"
                    |> Expect.equal (Err (InvalidCharacter '@'))
        , fuzz nonNegativeInt "round trip" <|
            \n ->
                encodeWithChecksum n
                    |> Result.andThen decodeWithChecksum
                    |> Expect.equal (Ok n)
        ]


nonNegativeInt =
    Fuzz.intRange 0 Random.maxInt
