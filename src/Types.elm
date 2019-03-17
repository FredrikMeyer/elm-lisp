module Types exposing (Either(..), Environment(..), Error(..), F(..), IntIntIntFunction, Sexp(..), Value(..))

import Dict exposing (Dict)


type Sexp
    = Val Value
    | ListOfSexps (List Sexp)
    | SexpError Error


type Error
    = ParserError String
    | ErrorMessage String
    | TypeError String



-- TODO: keyword type (typ def, let osv)


type Value
    = Integer Int
    | Symbol String
    | Function F
    | ValueError Error



-- ENVIRONMENT


type Environment
    = Environment
        { vars : Frame
        , outer : Maybe Environment
        }


type alias Frame =
    Dict String Value


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
