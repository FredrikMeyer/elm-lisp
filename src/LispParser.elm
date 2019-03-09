module LispParser exposing (Sexp(..), Value(..), parseSexp, parseValue)

import Parser exposing (..)
import Set


type Sexp
    = Val Value
    | ListOfSexps (List Sexp)
    | SexpError (List DeadEnd)


type Value
    = Integer Int
    | Str String
    | ValueError (List DeadEnd)


parseSexp : String -> Sexp
parseSexp text =
    let
        expr =
            Parser.run sexp text
    in
    case expr of
        Ok sexp_ ->
            sexp_

        Err error ->
            SexpError error


parseValue : String -> Value
parseValue text =
    let
        expr =
            Parser.run value text
    in
    case expr of
        Ok val ->
            val

        Err error ->
            ValueError error


sexp : Parser Sexp
sexp =
    oneOf
        [ map Val value
        , succeed ListOfSexps
            |. symbol "("
            |. spaces
            |= sexpsList
            |. symbol ")"
        ]


sexpsList : Parser (List Sexp)
sexpsList =
    loop [] sexpHelper


sexpHelper : List Sexp -> Parser (Step (List Sexp) (List Sexp))
sexpHelper revSexps =
    oneOf
        [ succeed (\stmt -> Loop (stmt :: revSexps))
            |= sexp
            |. spaces
        , succeed ()
            |> map (\_ -> Done (List.reverse revSexps))
        ]


value : Parser Value
value =
    oneOf
        [ map Integer int
        , map Str <|
            variable
                { start = \c -> Char.isAlphaNum c || (c /= '(' && c /= ')' && c /= ' ')
                , inner = \c -> Char.isAlphaNum c || (c /= '(' && c /= ')' && c /= ' ')
                , reserved = Set.fromList []
                }
        ]
