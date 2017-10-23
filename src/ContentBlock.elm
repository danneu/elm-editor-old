module ContentBlock exposing (..)

{-| A ContentBlock is a top-level block element
-}

import Json.Encode as JE
import Util exposing ((=>))


type alias ContentBlock =
    { key : String
    , text : String
    }


textLength : ContentBlock -> Int
textLength { text } =
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
