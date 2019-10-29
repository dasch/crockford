module Main exposing (main)

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Crockford


main : BenchmarkProgram
main =
    program suite


maxInt =
    2 ^ 30 - 1


suite : Benchmark
suite =
    describe "Crockford.encode"
        [ benchmark "small integer" <|
            \_ -> Crockford.encode 7
        , benchmark "large integer" <|
            \_ -> Crockford.encode maxInt
        ]
