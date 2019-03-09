module Printer exposing (toString)

import LispParser exposing (..)


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

        SexpError _ ->
            " [ ERROR READING SEXP ]"


value : Value -> String
value val =
    case val of
        Integer i ->
            String.fromInt i

        Str s ->
            s

        ValueError _ ->
            "SOME ERROR"
