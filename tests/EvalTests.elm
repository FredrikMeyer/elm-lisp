module EvalTests exposing (simpleArithmetic, suite)

import Environment exposing (..)
import Eval
import Expect exposing (Expectation)
import LispParser exposing (..)
import Printer
import Test exposing (..)
import Types exposing (..)


suite : Test
suite =
    describe "Test eval"
        [ simpleArithmetic
        , handleFloat
        ]


simpleArithmetic : Test
simpleArithmetic =
    describe "simple arithmetic"
        [ test "(/ (- (+ 5 (* 2 3)) 3) 4)" <|
            \_ ->
                let
                    env =
                        initialEnvironment

                    sexp =
                        parseSexp "(/ (- (+ 5 (* 2 3)) 3) 4)"

                    evaluated =
                        Eval.eval env sexp
                in
                case evaluated of
                    ( _, Val (Float_ a) ) ->
                        if round a == 2 then
                            Expect.pass

                        else
                            Expect.fail <| "Wrong answer, got " ++ String.fromFloat a

                    _ ->
                        Expect.fail <| "Expected 2, got " ++ Printer.toString (Tuple.second evaluated)
        ]
