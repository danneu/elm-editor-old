module Util exposing (..)


unwrapMaybe : Maybe a -> a
unwrapMaybe maybe =
    case maybe of
        Nothing ->
            Debug.crash "Tried to unwrap a Just"

        Just value ->
            value


{-| If string is empty, replaces it with space.

Using this as a quick hack since right now empty content blocks
cannot be selected since they have no text nodes.

-}
orSpace : String -> String
orSpace string =
    if String.isEmpty string then
        " "
    else
        string


(=>) : a -> b -> ( a, b )
(=>) =
    (,)
