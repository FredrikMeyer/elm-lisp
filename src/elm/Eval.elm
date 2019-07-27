module Eval exposing (eval)

import Dict exposing (Dict, get)
import Environment
import LispParser
import Printer
import Result exposing (Result(..))
import Types exposing (..)
import Util exposing (filterMaybe)


eval : Environment -> Sexp -> ( Environment, Sexp )
eval env sexp =
    case sexp of
        Val v ->
            case v of
                Symbol str ->
                    case Environment.get str env of
                        Nothing ->
                            ( env, SexpError (ErrorMessage ("Unknown var: " ++ str)) )

                        Just var ->
                            ( env, Val var )

                _ ->
                    ( env, sexp )

        ListOfSexps l ->
            case l of
                [] ->
                    ( env, SexpError (ErrorMessage "() is illegal.") )

                f :: args ->
                    case f of
                        Val (Symbol "def!") ->
                            case args of
                                [ Val (Symbol key), val ] ->
                                    let
                                        evaluatedVal =
                                            toValue env val
                                    in
                                    ( Environment.set key evaluatedVal env
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

                                        extendedEnv =
                                            Environment.extend env
                                                |> Environment.set key evaluatedVal

                                        ( newEnv, res ) =
                                            eval extendedEnv rest
                                    in
                                    ( env, res )

                                _ ->
                                    ( env, SexpError <| ErrorMessage "Syntax error" )

                        Val (Symbol "fn") ->
                            case args of
                                [ ListOfSexps a, rest ] ->
                                    let
                                        values =
                                            List.map (toValue env) a
                                                |> List.map toSymbolString
                                                |> filterMaybe

                                        extendedEnv =
                                            Environment.extend env

                                        function =
                                            case values of
                                                Nothing ->
                                                    -- TODO Check for distinct values
                                                    ValueError <| TypeError "Parameter must be string"

                                                Just strings ->
                                                    Function <|
                                                        UserFunction
                                                            { body = rest
                                                            , env = extendedEnv
                                                            , params = strings
                                                            }
                                    in
                                    ( env
                                    , Val <| function
                                    )

                                _ ->
                                    ( env, SexpError <| ErrorMessage "Syntax Error" )

                        Val (Symbol "do") ->
                            let
                                bOp =
                                    \s -> \envSexpTuple -> eval (Tuple.first envSexpTuple) s

                                newEnvSexpTuple =
                                    List.foldl bOp ( env, sexp ) args
                            in
                            newEnvSexpTuple

                        Val (Symbol "if") ->
                            case args of
                                [ cond, ifTrue, ifFalse ] ->
                                    let
                                        evalCond =
                                            toValue env cond
                                    in
                                    case evalCond of
                                        Boolean False ->
                                            eval env ifFalse

                                        Nil ->
                                            eval env ifFalse

                                        _ ->
                                            eval env ifTrue

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



-- TODO Denne er en mellomting mellom apply og en konvertør.
-- Burde kun konvertere, ikke kalle på eval


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


toSymbolString : Value -> Maybe String
toSymbolString val =
    case val of
        Symbol s ->
            Just s

        _ ->
            Nothing


apply : Value -> List Value -> Value
apply val args =
    case val of
        Function f ->
            case f of
                NumericFloat function ->
                    let
                        applied =
                            applyFloatFunction function function.init args
                    in
                    case applied of
                        Err e ->
                            ValueError e

                        Ok b ->
                            Float_ b

                FloatFloatBoolFunction function ->
                    case args of
                        [ Float_ a, Float_ b ] ->
                            Boolean <| function.f a b

                        _ ->
                            ValueError <| ErrorMessage "< only accepts numbers."

                UserFunction { env, params, body } ->
                    let
                        updatedEnv =
                            List.foldr
                                (\a -> \newEnv -> Environment.set a.name a.val newEnv)
                                env
                                (List.map2
                                    (\a ->
                                        \b ->
                                            { val = a
                                            , name = b
                                            }
                                    )
                                    args
                                    params
                                )

                        evaluated =
                            eval updatedEnv body
                    in
                    toValue updatedEnv (Tuple.second evaluated)

        Boolean _ ->
            ValueError <| TypeError "Cannot apply bool"

        Float_ _ ->
            ValueError <| TypeError "Cannot apply float."

        Nil ->
            ValueError <| TypeError "Cannot apply nil."

        Symbol _ ->
            ValueError <| TypeError "Cannot apply symbol."

        ValueError _ ->
            val


applyFunction : F -> List Value -> Result Error Value
applyFunction f args =
    Err <| ErrorMessage "Not all ints"


applyFloatFunction : FloatFloatFloatFunction -> Float -> List Value -> Result Error Float
applyFloatFunction f init args =
    case args of
        [] ->
            Ok init

        h :: rest ->
            case h of
                Float_ i ->
                    applyFloatFunction f (f.f init i) rest

                _ ->
                    Err <| TypeError "Not a number."
