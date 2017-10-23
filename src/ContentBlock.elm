module ContentBlock exposing (..)

{-| A ContentBlock is a top-level block element
-}

import Json.Encode as JE
import Util exposing ((=>))


type alias ContentBlock =
    { key : String
    , text : String
    }



-- HACK: Hacking around the fact that empty divs current get empty space


textLength : ContentBlock -> Int
textLength { text } =
    if text == " " then
        0
    else
        String.length text


isEmpty : ContentBlock -> Bool
isEmpty { text } =
    String.isEmpty text



-- JSON


encode : ContentBlock -> JE.Value
encode block =
    JE.object
        [ "key" => JE.string block.key
        , "text" => JE.string block.text
        ]
