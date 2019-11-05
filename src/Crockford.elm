module Crockford exposing
    ( encode, decode
    , encodeWithChecksum, decodeWithChecksum
    )

{-| Encode integers as [Crockford-style base32 strings](https://www.crockford.com/base32.html).

From the specification:

> Base 32 is a textual 32-symbol notation for expressing numbers in a form that can be conveniently and accurately transmitted between humans and computer systems. It can be used for out of band communication of public keys.
>
> The encoding scheme is required to
>
>   - Be human readable and machine readable.
>   - Be compact. Humans have difficulty in manipulating long strings of arbitrary symbols.
>   - Be error resistant. Entering the symbols must not require keyboarding gymnastics.
>   - Be pronounceable. Humans should be able to accurately transmit the symbols to other humans using a telephone.

This package provides functions for encoding and decoding base32 data.


# Encoding & decoding

@docs encode, decode


# Checksums

You can optionally insert a checksum at the end of the encoded data.

@docs encodeWithChecksum, decodeWithChecksum

-}


{-| Encode an integer as a base32 string.

    Crockford.encode 1337 --> "19S" : String

-}
encode : Int -> Result String String
encode x =
    encodeAdvanced x False


encodeAdvanced : Int -> Bool -> Result String String
encodeAdvanced x checksum =
    let
        encodeAsCharList : Int -> List Char
        encodeAsCharList n =
            let
                div =
                    n // 32

                rem =
                    modBy 32 n
            in
            if div > 0 then
                encodeSmallInt rem :: encodeAsCharList div

            else
                [ encodeSmallInt rem ]

        insertChecksum : List Char -> List Char
        insertChecksum chars =
            if checksum then
                encodeSmallInt (checksumOf x) :: chars

            else
                chars
    in
    if x < 0 then
        Err "cannot encode a negative number"

    else
        encodeAsCharList x
            |> insertChecksum
            |> List.reverse
            |> String.fromList
            |> Ok


{-| Decode a base32 string to an integer.

    Crockford.decode "19S" --> 1337 : Int

-}
decode : String -> Result String Int
decode s =
    let
        integrateChar chr numOrErr =
            case numOrErr of
                Ok num ->
                    if chr == '-' then
                        Ok num

                    else
                        let
                            encodedChar =
                                decodeChar chr
                        in
                        if encodedChar < 0 then
                            Err ("invalid base32 character `" ++ String.fromChar chr ++ "`")

                        else
                            Ok (num * 32 + encodedChar)

                Err err ->
                    Err err
    in
    String.toUpper s
        |> String.toList
        |> List.foldl integrateChar (Ok 0)


encodeSmallInt : Int -> Char
encodeSmallInt n =
    case n of
        -- Symbol set:
        0 ->
            '0'

        1 ->
            '1'

        2 ->
            '2'

        3 ->
            '3'

        4 ->
            '4'

        5 ->
            '5'

        6 ->
            '6'

        7 ->
            '7'

        8 ->
            '8'

        9 ->
            '9'

        10 ->
            'A'

        11 ->
            'B'

        12 ->
            'C'

        13 ->
            'D'

        14 ->
            'E'

        15 ->
            'F'

        16 ->
            'G'

        17 ->
            'H'

        18 ->
            'J'

        19 ->
            'K'

        20 ->
            'M'

        21 ->
            'N'

        22 ->
            'P'

        23 ->
            'Q'

        24 ->
            'R'

        25 ->
            'S'

        26 ->
            'T'

        27 ->
            'V'

        28 ->
            'W'

        29 ->
            'X'

        30 ->
            'Y'

        31 ->
            'Z'

        -- Checksum symbols:
        32 ->
            '*'

        33 ->
            '~'

        34 ->
            '$'

        35 ->
            '='

        36 ->
            'U'

        -- Out of bounds:
        _ ->
            '#'


decodeChar : Char -> Int
decodeChar n =
    case n of
        -- Special cases:
        'I' ->
            1

        'L' ->
            1

        'O' ->
            0

        -- General cases:
        '0' ->
            0

        '1' ->
            1

        '2' ->
            2

        '3' ->
            3

        '4' ->
            4

        '5' ->
            5

        '6' ->
            6

        '7' ->
            7

        '8' ->
            8

        '9' ->
            9

        'A' ->
            10

        'B' ->
            11

        'C' ->
            12

        'D' ->
            13

        'E' ->
            14

        'F' ->
            15

        'G' ->
            16

        'H' ->
            17

        'J' ->
            18

        'K' ->
            19

        'M' ->
            20

        'N' ->
            21

        'P' ->
            22

        'Q' ->
            23

        'R' ->
            24

        'S' ->
            25

        'T' ->
            26

        'V' ->
            27

        'W' ->
            28

        'X' ->
            29

        'Y' ->
            30

        'Z' ->
            31

        -- Checksum symbols:
        '*' ->
            32

        '~' ->
            33

        '$' ->
            34

        '=' ->
            35

        'U' ->
            36

        -- Out of bounds:
        _ ->
            -1



-- Checksums


checksumBase : Int
checksumBase =
    37


checksumOf : Int -> Int
checksumOf n =
    modBy checksumBase n


encodeWithChecksum : Int -> Result String String
encodeWithChecksum x =
    encodeAdvanced x True


decodeWithChecksum : String -> Result String Int
decodeWithChecksum s =
    let
        encodedData =
            String.dropRight 1 s

        encodedChecksum =
            String.right 1 s

        validateChecksum n checksum =
            if checksumOf n == checksum then
                Ok n

            else
                Err "invalid checksum"
    in
    decode encodedData
        |> Result.andThen
            (\data ->
                decode encodedChecksum
                    |> Result.andThen
                        (\checksum ->
                            validateChecksum data checksum
                        )
            )
