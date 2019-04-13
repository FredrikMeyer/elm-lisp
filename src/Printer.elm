module Printer exposing (toString)

import Types exposing (Error(..), F(..), Sexp(..), Value(..))


toString : Sexp -> String
toString sexp =
    case sexp of
        Val val ->
            value val

        ListOfSexps l ->
            let
                toStrings =
                    List.map toString l

                conc =
                    String.join " " toStrings
            in
            "(" ++ conc ++ ")"

        SexpError error ->
            case error of
                ErrorMessage message ->
                    "ERROR: " ++ message

                ParserError message ->
                    "PARSING ERROR: " ++ message

                TypeError message ->
                    "TYPE ERROR: " ++ message


value : Value -> String
value val =
    case val of
        Boolean b ->
            case b of
                True ->
                    "#t"

                False ->
                    "#f"

        Integer i ->
            String.fromInt i

        Float_ f ->
            String.fromFloat f

        Nil ->
            "nil"

        Symbol s ->
            "SYMBOL: " ++ s

        Function f ->
            case f of
                NumericInt function ->
                    "`function: " ++ function.name ++ "`"

                NumericFloat function ->
                    "`function: " ++ function.name ++ "`"

        ValueError e ->
            case e of
                TypeError s ->
                    "TYPERROR: " ++ s

                ErrorMessage s ->
                    "ERROR: " ++ s

                ParserError _ ->
                    "PARSE ERROR"
