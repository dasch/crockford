module Crockford.Advanced exposing (Error(..), decode, encode)

{-| Encoding or decoding can fail in a couple of ways:

  - `NegativeNumberError` means you tried to encode a negative integer, which isn't supported.
  - `InvalidChecksum` means you tried to decode a base32 string with a checksum, but the checksum didn't match.
  - `InvalidCharacter` means you tried to decode a base32 string, but an invalid character was encountered.
  - `EmptyString` means you tried to decode an empty string.

-}


type Error
    = NegativeNumberError
    | InvalidChecksum
    | InvalidCharacter Char
    | EmptyString
    | NumberTooLarge Int


encode : { checksum : Bool } -> Int -> Result Error String
encode { checksum } x =
    let
        encodeAsCharList : Int -> List Char
        encodeAsCharList n =
            let
                div =
                    n // 32

                rem =
                    remainderBy 32 n
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

        combineAndReverse : List Char -> String
        combineAndReverse chars =
            List.foldl String.cons "" chars
    in
    if x < 0 then
        Err NegativeNumberError

    else if x // 32 < 0 then
        Err (NumberTooLarge x)

    else
        encodeAsCharList x
            |> insertChecksum
            |> combineAndReverse
            |> Ok


decode : { checksum : Bool } -> String -> Result Error Int
decode { checksum } s =
    let
        decodeChars : Int -> List Char -> Result Error Int
        decodeChars curr chars =
            case chars of
                [] ->
                    Ok curr

                '-' :: cs ->
                    decodeChars curr cs

                [ c ] ->
                    if checksum then
                        validateChecksum (decodeChar c) curr

                    else
                        let
                            n =
                                decodeChar c
                        in
                        if n < 0 || n > 31 then
                            Err (InvalidCharacter c)

                        else
                            Ok (curr * 32 + n)

                c :: cs ->
                    let
                        n =
                            decodeChar c
                    in
                    if n < 0 || n > 31 then
                        Err (InvalidCharacter c)

                    else
                        decodeChars (curr * 32 + n) cs
    in
    if s == "" then
        Err EmptyString

    else
        String.toList s
            |> decodeChars 0



-- Character encoding/decoding.


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

        'i' ->
            1

        'L' ->
            1

        'l' ->
            1

        'O' ->
            0

        'o' ->
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

        'a' ->
            10

        'B' ->
            11

        'b' ->
            11

        'C' ->
            12

        'c' ->
            12

        'D' ->
            13

        'd' ->
            13

        'E' ->
            14

        'e' ->
            14

        'F' ->
            15

        'f' ->
            15

        'G' ->
            16

        'g' ->
            16

        'H' ->
            17

        'h' ->
            17

        'J' ->
            18

        'j' ->
            18

        'K' ->
            19

        'k' ->
            19

        'M' ->
            20

        'm' ->
            20

        'N' ->
            21

        'n' ->
            21

        'P' ->
            22

        'p' ->
            22

        'Q' ->
            23

        'q' ->
            23

        'R' ->
            24

        'r' ->
            24

        'S' ->
            25

        's' ->
            25

        'T' ->
            26

        't' ->
            26

        'V' ->
            27

        'v' ->
            27

        'W' ->
            28

        'w' ->
            28

        'X' ->
            29

        'x' ->
            29

        'Y' ->
            30

        'y' ->
            30

        'Z' ->
            31

        'z' ->
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

        'u' ->
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


validateChecksum : Int -> Int -> Result Error Int
validateChecksum checksum n =
    if checksumOf n == checksum then
        Ok n

    else
        Err InvalidChecksum
