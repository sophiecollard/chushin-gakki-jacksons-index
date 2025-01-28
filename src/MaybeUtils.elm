module MaybeUtils exposing (..)


toList : Maybe a -> List a
toList maybe =
    case maybe of
        Nothing ->
            []

        Just a ->
            [ a ]
