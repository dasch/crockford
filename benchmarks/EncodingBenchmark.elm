module EncodingBenchmark exposing (main)

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Crockford


main : BenchmarkProgram
main =
    program suite


maxInt =
    2 ^ 30 - 1


maxIntEncoded =
    Crockford.encode maxInt
        |> Result.withDefault ""


maxIntEncodedWithChecksum =
    Crockford.encodeWithChecksum maxInt
        |> Result.withDefault ""


suite : Benchmark
suite =
    describe "Crockford"
        [ describe "encode"
            [ benchmark "small integer" <|
                \_ -> Crockford.encode 7
            , benchmark "large integer" <|
                \_ -> Crockford.encode maxInt
            ]
        , describe "decode"
            [ benchmark "large integer" <|
                \_ -> Crockford.decode maxIntEncoded
            ]
        , describe "decodeWithChecksum"
            [ benchmark "large integer" <|
                \_ -> Crockford.decodeWithChecksum maxIntEncodedWithChecksum
            ]
        ]
