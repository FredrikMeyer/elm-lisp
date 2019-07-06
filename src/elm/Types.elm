module Types exposing (Either(..), Environment(..), Error(..), F(..), FloatFloatFloatFunction, Sexp(..), Value(..))

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
    = Float_ Float
    | Nil
    | Symbol String
    | Boolean Bool
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
    = NumericFloat FloatFloatFloatFunction


type alias FloatFloatFloatFunction =
    { f : Float -> Float -> Float
    , init : Float
    , name : String
    }



--


type Either a b
    = Left a
    | Right b
