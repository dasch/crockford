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
                Expect.equal "16J" (encode 1234)
        , fuzz nonNegativeInt "round trip" <|
            \n -> Expect.equal (Ok n) (decode (encode n))
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
                Expect.equal (Err "invalid base32 character `#`") (decode "123#")
        , fuzz nonNegativeInt "decoding is case insensitive" <|
            \n ->
                let
                    encoded =
                        encode n
                in
                Expect.equal (decode (String.toUpper encoded)) (decode (String.toLower encoded))
        ]


checksumTests =
    describe "checksums"
        [ test "encoding with checksum" <|
            \_ ->
                encodeWithChecksum 32
                    |> Expect.equal "10*"
        , test "decoding with checksum" <|
            \_ ->
                decodeWithChecksum "1A5"
                    |> Expect.equal (Ok 42)
        , test "failing on invalid checksum symbol" <|
            \_ ->
                decodeWithChecksum "1A@"
                    |> Expect.equal (Err "invalid base32 character `@`")
        , fuzz nonNegativeInt "round trip" <|
            \n ->
                encodeWithChecksum n
                    |> decodeWithChecksum
                    |> Expect.equal (Ok n)
        ]


nonNegativeInt =
    Fuzz.intRange 0 Random.maxInt
