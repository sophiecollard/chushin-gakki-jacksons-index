module ListUtils exposing (..)


nonEmpty : List a -> Bool
nonEmpty list =
    case list of
        [] ->
            False

        _ ->
            True
