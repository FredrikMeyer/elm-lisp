module LispParser exposing (parseSexp, parseValue)

import Parser exposing (..)
import Set
import Types exposing (..)



{--

Builds an AST.
--}


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
            Debug.toString error
                |> ParserError
                |> SexpError


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
            ValueError <| ParserError (Debug.toString error)


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
        [ number
            { float = Just Float_
            , binary = Nothing
            , hex = Nothing
            , octal = Nothing
            , int = Just (Float_ << toFloat)
            }
        , map (\_ -> Boolean True) (keyword "true")
        , map (\_ -> Boolean True) (keyword "#t")
        , map (\_ -> Boolean False) (keyword "false")
        , map (\_ -> Boolean False) (keyword "#f")
        , map (\_ -> Nil) (keyword "nil")
        , map Symbol <|
            variable
                { start = \c -> Char.isAlphaNum c || (c /= '(' && c /= ')' && c /= ' ')
                , inner = \c -> Char.isAlphaNum c || (c /= '(' && c /= ')' && c /= ' ')
                , reserved = Set.fromList []
                }
        ]
