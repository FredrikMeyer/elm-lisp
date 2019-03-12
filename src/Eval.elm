module Eval exposing (evalString)

import Dict exposing (Dict, get)
import LispParser
import Printer
import Result exposing (Result(..))
import Types exposing (..)


eval : Environment -> Sexp -> Sexp
eval env sexp =
    case sexp of
        Val v ->
            case v of
                Symbol str ->
                    let
                        maybeVar =
                            Dict.get str env.vars
                    in
                    case maybeVar of
                        Nothing ->
                            SexpError (ErrorMessage "Unknown var")

                        Just var ->
                            Val var

                _ ->
                    sexp

        ListOfSexps l ->
            case l of
                [] ->
                    SexpError (ErrorMessage "() is illegal.")

                f :: args ->
                    Val <|
                        apply (toValue env <| eval env f) <|
                            List.map (toValue env) args

        SexpError _ ->
            SexpError (ErrorMessage "Something wrong happened")


toValue : Environment -> Sexp -> Value
toValue env sexp =
    case sexp of
        Val v ->
            v

        ListOfSexps _ ->
            let
                evaluated =
                    eval env sexp
            in
            case evaluated of
                Val v ->
                    v

                ListOfSexps l ->
                    ValueError <| ErrorMessage "Should be val :/"

                SexpError error ->
                    ValueError error

        SexpError e ->
            ValueError e


evalString : Environment -> String -> String
evalString environment inp =
    let
        parsed =
            LispParser.parseSexp inp
    in
    eval environment parsed
        |> Printer.toString


apply : Value -> List Value -> Value
apply sexp args =
    case sexp of
        Function f ->
            case f of
                Numeric function ->
                    let
                        applied =
                            applyNumericFunction function function.init args
                    in
                    case applied of
                        Err e ->
                            ValueError e

                        Ok b ->
                            Integer b

        --                    Integer 1
        _ ->
            ValueError <| ErrorMessage "Cannot eval value"


applyFunction : F -> List Value -> Result Error Value
applyFunction f args =
    Err <| ErrorMessage "Not all ints"


applyNumericFunction : IntIntIntFunction -> Int -> List Value -> Result Error Int
applyNumericFunction f init args =
    case args of
        [] ->
            Ok init

        h :: rest ->
            case h of
                Integer i ->
                    applyNumericFunction f (f.f init i) rest

                _ ->
                    Err <| TypeError "Not an int."
