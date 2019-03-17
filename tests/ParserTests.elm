module ParserTests exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import LispParser exposing (..)
import Parser
import Test exposing (..)
import Types exposing (..)


suite : Test
suite =
    describe "Test parsers"
        [ canParseValue
        , canParseSexp
        ]


canParseSexp : Test
canParseSexp =
    describe "Can parse sexps"
        [ test "value" <|
            \_ ->
                let
                    inp =
                        "5"

                    parsed : Sexp
                    parsed =
                        parseSexp inp
                in
                case parsed of
                    Val _ ->
                        Expect.pass

                    ListOfSexps _ ->
                        Expect.fail "not a list"

                    SexpError err ->
                        Expect.fail (Debug.toString err)
        , test "parens are not allowed variable names" <|
            \_ ->
                let
                    inp =
                        "("

                    parsed =
                        parseSexp inp
                in
                case parsed of
                    SexpError err ->
                        Debug.log (Debug.toString err) Expect.pass

                    Val v ->
                        Debug.log (Debug.toString v) Expect.fail "Should fail"

                    _ ->
                        Expect.fail "Should fail!"
        , test "list of integers" <|
            \_ ->
                let
                    inp =
                        "(0 1 2 3)"

                    parsed =
                        Debug.log "res list of ints: " (parseSexp inp)
                in
                case parsed of
                    Val _ ->
                        Expect.fail "Not a value"

                    ListOfSexps l ->
                        Expect.equal (List.length l) 4

                    SexpError err ->
                        Expect.fail (Debug.toString err)
        , test "a single evaluated function" <|
            \_ ->
                let
                    inp =
                        "(f)"

                    parsed =
                        parseSexp inp
                in
                case parsed of
                    ListOfSexps l ->
                        Expect.equal (List.length l) 1

                    _ ->
                        Expect.fail (Debug.toString parsed)
        , test "list of vals" <|
            \_ ->
                let
                    inp =
                        "(f a b x)"

                    parsed =
                        Debug.log "res: " (parseSexp inp)
                in
                case parsed of
                    Val _ ->
                        Expect.fail "Not a value"

                    ListOfSexps l ->
                        Expect.equal (List.length l) 4

                    SexpError err ->
                        Expect.fail (Debug.toString err)
        , test "nested sexps" <|
            \_ ->
                let
                    inp =
                        "(+ (+ 1 2 3) 2 3)"

                    parsed =
                        Debug.log "nested sexps: " (parseSexp inp)
                in
                case parsed of
                    ListOfSexps l ->
                        Expect.equal (List.length l) 4

                    v ->
                        Expect.fail (Debug.toString v)
        , test "unbalanced parens should not parse" <|
            \_ ->
                let
                    inp =
                        "(+ 1 2 3)))"

                    parsed =
                        Debug.log "unb " (parseSexp inp)
                in
                Expect.pass
        ]


canParseValue : Test
canParseValue =
    describe "Can parse value"
        [ test "number" <|
            \_ ->
                let
                    inp =
                        "55"

                    parsed =
                        parseValue inp
                in
                case parsed of
                    Integer 55 ->
                        Expect.pass

                    _ ->
                        Expect.fail "Should parse to int 55"
        , test "variable" <|
            \_ ->
                let
                    inp =
                        "hei"

                    parsed =
                        parseValue inp
                in
                case parsed of
                    Symbol s ->
                        Expect.equal s inp

                    _ ->
                        Expect.fail "Should parse to string \"hei\""
        , test "variable with spaces" <|
            \_ ->
                let
                    inp =
                        "hei du"

                    parsed =
                        parseValue inp
                in
                case parsed of
                    Symbol s ->
                        Expect.equal "hei" s

                    _ ->
                        Expect.fail "Failed"
        , test "disallowed character" <|
            \_ ->
                let
                    inp =
                        "(f)"

                    parsed =
                        parseValue inp
                in
                case parsed of
                    ValueError deadEndList ->
                        Expect.pass

                    v ->
                        Expect.fail (Debug.toString v)
        , test "+ as variable" <|
            \_ ->
                let
                    inp =
                        "+"

                    parsed =
                        parseValue inp
                in
                case parsed of
                    Symbol "+" ->
                        Expect.pass

                    v ->
                        Expect.fail (Debug.toString v)
        ]
