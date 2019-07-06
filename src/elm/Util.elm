module Util exposing (filterMaybe)

import List exposing (foldr)


combineMaybes : Maybe a -> Maybe (List a) -> Maybe (List a)
combineMaybes ma la =
    case ma of
        Nothing ->
            Nothing

        Just a ->
            case la of
                Nothing ->
                    Nothing

                Just l ->
                    Just (a :: l)


filterMaybe : List (Maybe a) -> Maybe (List a)
filterMaybe l =
    foldr combineMaybes (Just []) l
