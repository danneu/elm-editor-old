module String.Extra exposing (..)

import String


removeIndex : Int -> String -> String
removeIndex index string =
    let
        left =
            string
                |> String.slice 0 index

        right =
            string
                |> String.slice (index + 1) (String.length string)
    in
        left ++ right


insertAfterIndex : Int -> String -> String -> String
insertAfterIndex index addition string =
    let
        left =
            string
                |> String.slice 0 index

        right =
            string
                |> String.slice index (String.length string)
    in
        left ++ addition ++ right


splitAtIndex : Int -> String -> ( String, String )
splitAtIndex index string =
    ( String.slice 0 index string, String.slice index (String.length string) string )
