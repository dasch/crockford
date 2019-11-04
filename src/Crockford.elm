module Crockford exposing (decode, decodeWithChecksum, encode, encodeWithChecksum)


encode : Int -> String
encode x =
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
    in
    encodeAsCharList x
        |> List.reverse
        |> String.fromList


decode : String -> Int
decode s =
    let
        integrateChar chr num =
            if chr == '-' then
                num

            else
                num * 32 + decodeChar chr
    in
    String.toUpper s
        |> String.toList
        |> List.foldl integrateChar 0


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


encodeWithChecksum : Int -> String
encodeWithChecksum x =
    let
        encoded =
            encode x

        checkSymbol =
            encodeSmallInt (checksumOf x)
                |> String.fromChar
    in
    String.append encoded checkSymbol


decodeWithChecksum : String -> Maybe Int
decodeWithChecksum s =
    let
        checksum =
            String.right 1 s
                |> decode

        n =
            String.dropRight 1 s
                |> decode
    in
    if checksumOf n == checksum then
        Just n

    else
        Nothing
