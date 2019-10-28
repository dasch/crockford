module Crockford exposing (decode, encode)


encode : Int -> String
encode n =
    let
        div =
            n // 32

        rem =
            modBy 32 n
    in
    if div > 0 then
        encode div ++ encodeSmallInt rem

    else
        encodeSmallInt rem


decode : String -> Int
decode s =
    String.toList s
        |> List.foldl (\chr num -> num * 32 + decodeSmallInt chr) 0


encodeSmallInt : Int -> String
encodeSmallInt n =
    case n of
        0 ->
            "0"

        1 ->
            "1"

        2 ->
            "2"

        3 ->
            "3"

        4 ->
            "4"

        5 ->
            "5"

        6 ->
            "6"

        7 ->
            "7"

        8 ->
            "8"

        9 ->
            "9"

        10 ->
            "A"

        11 ->
            "B"

        12 ->
            "C"

        13 ->
            "D"

        14 ->
            "E"

        15 ->
            "F"

        16 ->
            "G"

        17 ->
            "H"

        18 ->
            "J"

        19 ->
            "K"

        20 ->
            "M"

        21 ->
            "N"

        22 ->
            "P"

        23 ->
            "Q"

        24 ->
            "R"

        25 ->
            "S"

        26 ->
            "T"

        27 ->
            "V"

        28 ->
            "W"

        29 ->
            "X"

        30 ->
            "Y"

        31 ->
            "Z"

        _ ->
            "#"


decodeSmallInt : Char -> Int
decodeSmallInt n =
    case n of
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

        _ ->
            0
