module SelectionState exposing (..)

import Json.Encode as JE
import Util exposing ((=>))


type alias SelectionState =
    { anchorKey : String
    , anchorOffset : Int
    , focusKey : String
    , focusOffset : Int

    -- , isBackward : Bool
    -- , hasFocus : Bool
    }


{-| Creates a collapsed selection which only exists in a single block.
-}
collapsed : String -> Int -> SelectionState
collapsed blockKey offset =
    { anchorKey = blockKey
    , focusKey = blockKey
    , anchorOffset = offset
    , focusOffset = offset
    }


increment : SelectionState -> SelectionState
increment state =
    { state | anchorOffset = state.anchorOffset + 1, focusOffset = state.focusOffset + 1 }


decrement : SelectionState -> SelectionState
decrement state =
    { state | anchorOffset = state.anchorOffset - 1, focusOffset = state.focusOffset - 1 }


isCollapsed : SelectionState -> Bool
isCollapsed state =
    state.anchorKey == state.focusKey && state.anchorOffset == state.focusOffset



-- JSON


encode : SelectionState -> JE.Value
encode selection =
    JE.object
        [ "anchorKey" => JE.string selection.anchorKey
        , "anchorOffset" => JE.int selection.anchorOffset
        , "focusKey" => JE.string selection.focusKey
        , "focusOffset" => JE.int selection.focusOffset
        ]
