module Environment exposing (extend, find, get, initialEnvironment, set)

import Dict
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
    find key env
        |> Maybe.andThen
            (\env_ ->
                case env_ of
                    Environment { vars } ->
                        Dict.get key vars
            )


extend : Environment -> Environment
extend env =
    Environment
        { vars = Dict.empty
        , outer = Just env
        }


emptyEnvironment : Environment
emptyEnvironment =
    Environment { vars = Dict.empty, outer = Nothing }


initialEnvironment : Environment
initialEnvironment =
    emptyEnvironment
        |> set "+"
            (Function <|
                NumericFloat
                    { f = \a -> \b -> a + b
                    , init = 0
                    , name = "PLUS"
                    }
            )
        |> set "*"
            (Function <|
                NumericFloat
                    { f = \a -> \b -> a * b
                    , init = 1
                    , name = "MULTIPLY"
                    }
            )
        |> set "-"
            (Function <|
                NumericFloat
                    { f = \a -> \b -> -a - b
                    , init = 0
                    , name = "SUBTRACT"
                    }
            )
        |> set "/"
            (Function <|
                NumericFloat
                    { f = \a -> \b -> (1 / a) / b
                    , init = 1
                    , name = "DIVIDE"
                    }
            )
        |> set "<"
            (Function <|
                FloatFloatBoolFunction
                    { f = \a -> \b -> a < b
                    , name = "LESS THAN"
                    }
            )
        |> set ">"
            (Function <|
                FloatFloatBoolFunction
                    { f = \a -> \b -> a > b
                    , name = "GREATER THAN"
                    }
            )
        |> set "="
            (Function <|
                FloatFloatBoolFunction
                    { f = \a -> \b -> a == b
                    , name = "EQUAL"
                    }
            )
