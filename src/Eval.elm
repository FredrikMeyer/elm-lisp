module Eval exposing (eval)

import Dict exposing (Dict, get)
import Environment
import LispParser
import Printer
import Result exposing (Result(..))
import Types exposing (..)


eval : Environment -> Sexp -> ( Environment, Sexp )
eval env sexp =
    case sexp of
        Val v ->
            case v of
                Symbol str ->
                    case Environment.get str env of
                        Nothing ->
                            ( env, SexpError (ErrorMessage "Unknown var") )

                        Just var ->
                            ( env, Val var )

                _ ->
                    ( env, sexp )

        ListOfSexps l ->
            case l of
                [] ->
                    ( env, SexpError (ErrorMessage "() is illegal.") )

                f :: args ->
                    case Debug.log "proc" f of
                        Val (Symbol "def!") ->
                            case Debug.log "args" args of
                                [ Val (Symbol key), val ] ->
                                    let
                                        evaluatedVal =
                                            toValue env val
                                    in
                                    ( Debug.log "newEnv" <| Environment.set key evaluatedVal env
                                    , Val evaluatedVal
                                    )

                                _ ->
                                    ( env, SexpError <| ErrorMessage "???" )

                        Val (Symbol "let") ->
                            case args of
                                [ ListOfSexps [ Val (Symbol key), v ], rest ] ->
                                    let
                                        evaluatedVal =
                                            toValue env v

                                        scopedEnv =
                                            Environment.set key evaluatedVal env

                                        ( newEnv, res ) =
                                            eval scopedEnv rest
                                    in
                                    ( env, res )

                                _ ->
                                    ( env, SexpError <| ErrorMessage "Syntax error" )

                        _ ->
                            let
                                proc =
                                    Tuple.second (eval env f)
                                        |> toValue env
                            in
                            ( env
                            , Val <|
                                apply (toValue env <| Tuple.second (eval env f)) <|
                                    (args
                                        |> List.map (eval env)
                                        |> List.map Tuple.second
                                        |> List.map (toValue env)
                                    )
                            )

        SexpError _ ->
            ( env, sexp )


toValue : Environment -> Sexp -> Value
toValue env sexp =
    case sexp of
        Val v ->
            v

        ListOfSexps _ ->
            let
                evaluated =
                    eval env sexp
                        |> Tuple.second
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

        _ ->
            ValueError <| ErrorMessage "Cannot eval value"


applyFunction : F -> List Value -> Result Error Value
applyFunction f args =
    Err <| ErrorMessage "Not all ints"


applyNumericFunction : IntIntIntFunction -> Int -> List Value -> Result Error Int
applyNumericFunction f init args =
    case Debug.log "args : " args of
        [] ->
            Ok init

        h :: rest ->
            case h of
                Integer i ->
                    applyNumericFunction f (f.f init i) rest

                _ ->
                    Err <| TypeError "Not an int."



--toList : List Sexp -> List (String, )
