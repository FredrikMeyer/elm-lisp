module Environment exposing (find, get, initialEnvironment, set)

import Dict exposing (Dict)
import Types exposing (..)


set : String -> Value -> Environment -> Environment
set name val env =
    case env of
        Environment { vars, outer } ->
            let
                newDict =
                    Dict.insert name val vars
            in
            Environment { vars = newDict, outer = outer }


find : String -> Environment -> Maybe Environment
find key env =
    case env of
        Environment { vars, outer } ->
            if Dict.member key vars then
                Just env

            else
                Maybe.andThen (find key) outer


get : String -> Environment -> Maybe Value
get key env =
    case find key env of
        Nothing ->
            Nothing

        Just (Environment { vars }) ->
            Dict.get key vars


emptyEnvironment : Environment
emptyEnvironment =
    Environment { vars = Dict.empty, outer = Nothing }


initialEnvironment : Environment
initialEnvironment =
    emptyEnvironment
        |> set "+"
            (Function <|
                Numeric
                    { f = \a -> \b -> a + b
                    , init = 0
                    , name = "PLUS"
                    }
            )
        |> set "*"
            (Function <|
                Numeric
                    { f = \a -> \b -> a * b
                    , init = 1
                    , name = "MULTIPLY"
                    }
            )
        |> set "-"
            (Function <|
                Numeric
                    { f = \a -> \b -> -a - b
                    , init = 0
                    , name = "SUBTRACT"
                    }
            )
