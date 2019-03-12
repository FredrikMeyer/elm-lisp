module Environment exposing (initialEnvironment)

import Dict exposing (Dict)
import Types exposing (..)


initialEnvironment : Environment
initialEnvironment =
    { vars =
        Dict.fromList
            [ ( "+"
              , Function <|
                    Numeric
                        { f = \a -> \b -> a + b
                        , init = 0
                        , name = "PLUS"
                        }
              )
            , ( "*"
              , Function <|
                    Numeric
                        { f = \a -> \b -> a * b
                        , init = 1
                        , name = "MULTIPLY"
                        }
              )
            , ( "-"
              , Function <|
                    Numeric
                        { f = \a -> \b -> -a - b
                        , init = 0
                        , name = "SUBTRACT"
                        }
              )
            ]
    }
