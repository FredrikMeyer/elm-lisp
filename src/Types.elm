module Types exposing (Either(..), Environment, Error(..), F(..), IntIntIntFunction, Sexp(..), Value(..))

import Dict exposing (Dict)
import Parser exposing (DeadEnd)


type Sexp
    = Val Value
    | ListOfSexps (List Sexp)
    | SexpError Error


type Error
    = ParserError (List DeadEnd)
    | ErrorMessage String
    | TypeError String


type Value
    = Integer Int
    | Symbol String
    | Function F
    | ValueError Error



-- ENVIRONMENT


type alias Environment =
    { vars : Dict String Value
    }


type F
    = Numeric IntIntIntFunction


type alias IntIntIntFunction =
    { f : Int -> Int -> Int
    , init : Int
    , name : String
    }



--


type Either a b
    = Left a
    | Right b
