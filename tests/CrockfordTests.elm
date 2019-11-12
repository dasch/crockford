module CrockfordTests exposing (suite)

import Bytes
import Bytes.Encode
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
        , bytesTests
        ]


encodingTests =
    describe "encoding"
        [ test "encodes integers" <|
            \_ ->
                Expect.equal (Ok "16J") (encode 1234)
        , test "fails on integers that are too large to be divided by 32" <|
            \_ ->
                let
                    n =
                        364432432434209
                in
                encode n
                    |> Expect.equal (Ok "ABEC4TW311")
        , test "fails on negative integers" <|
            \_ ->
                Expect.equal (Err NegativeNumber) (encode -1)
        , fuzz nonNegativeInt "round trip" <|
            \n ->
                encode n
                    |> Result.andThen decode
                    |> Expect.equal (Ok n)
        , fuzz Fuzz.string "decoding random strings" <|
            \str ->
                case decode str of
                    Err _ ->
                        Expect.pass

                    Ok n ->
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
                    |> Expect.equal (Err InvalidChecksum)
        , fuzz nonNegativeInt "round trip" <|
            \n ->
                encodeWithChecksum n
                    |> Result.andThen decodeWithChecksum
                    |> Expect.equal (Ok n)
        ]


bytesTests =
    describe "Bytes"
        [ fuzz unsignedInt32 "encoding unsigned 32 bit integers as Bytes" <|
            \n ->
                Bytes.Encode.encode (Bytes.Encode.unsignedInt32 Bytes.BE n)
                    |> encodeBytes
                    |> Result.andThen decode
                    |> Expect.equal (Ok n)
        , fuzz Fuzz.string "encoding arbitrary Bytes" <|
            \str ->
                Bytes.Encode.sequence
                    [ Bytes.Encode.unsignedInt32 Bytes.BE (String.length str)
                    , Bytes.Encode.string str
                    ]
                    |> Bytes.Encode.encode
                    |> encodeBytes
                    |> Result.andThen decode
                    |> Expect.equal (Ok 13)
        ]


nonNegativeInt =
    Fuzz.intRange 0 Random.maxInt


unsignedInt32 =
    Fuzz.intRange 0 4294967295
