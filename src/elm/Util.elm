module Util exposing (filterMaybe)

import List exposing (foldr)


combineMaybes : Maybe a -> Maybe (List a) -> Maybe (List a)
combineMaybes ma la =
    Maybe.andThen
        (\a -> Maybe.map (\l -> a :: l) la)
        ma


filterMaybe : List (Maybe a) -> Maybe (List a)
filterMaybe l =
    foldr combineMaybes (Just []) l
