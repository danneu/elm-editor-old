module EditorState exposing (..)

import Array
import Keyboard.Extra exposing (Key)
import SelectionState exposing (SelectionState)
import Char
import Array exposing (Array)
import Util
import ContentBlock exposing (ContentBlock)
import String.Extra
import Util exposing ((=>))
import Json.Encode as JE
import Keyboard.Extra
import OrderedDict exposing (OrderedDict)


type alias EditorState =
    { blocks : OrderedDict String ContentBlock
    , pressedKeys : List Key
    , selection : SelectionState
    , id : Int
    }


empty : EditorState
empty =
    let
        block1 =
            ContentBlock "1" "a"

        block2 =
            ContentBlock "2" "b"

        -- NOTE: Without Util.orSpace on these next two items, you cannot
        -- up/down arrow key to select them.
        block3 =
            ContentBlock "3" ""

        block4 =
            ContentBlock "4" ""

        block5 =
            ContentBlock "5" "c"
    in
        { blocks =
            OrderedDict.fromList
                [ ( block1.key, block1 )
                , ( block2.key, block2 )
                , ( block3.key, block3 )
                , ( block4.key, block4 )
                , ( block5.key, block5 )
                ]
        , pressedKeys = []
        , selection = SelectionState.collapsed block2.key (String.length block2.text)
        , id = 100
        }


updateSelection : (SelectionState -> SelectionState) -> EditorState -> EditorState
updateSelection update editor =
    { editor | selection = update editor.selection }


{-| Applies a function to the block of the given key.
-}
updateBlock : String -> (ContentBlock -> ContentBlock) -> EditorState -> EditorState
updateBlock key update editor =
    case OrderedDict.get key editor.blocks of
        Nothing ->
            editor

        Just prevBlock ->
            { editor | blocks = OrderedDict.update key update editor.blocks }


setSelection : SelectionState -> EditorState -> EditorState
setSelection selection editor =
    { editor | selection = selection }


setBlocks : OrderedDict String ContentBlock -> EditorState -> EditorState
setBlocks blocks editor =
    { editor | blocks = blocks }


getBlockBeforeKey : String -> EditorState -> Maybe ContentBlock
getBlockBeforeKey key editor =
    OrderedDict.getBefore key editor.blocks


removeBlock : String -> EditorState -> EditorState
removeBlock key editor =
    let
        nextBlocks =
            OrderedDict.remove key editor.blocks
    in
        if OrderedDict.size nextBlocks == OrderedDict.size editor.blocks then
            -- Block not found, nothing was changed
            editor
        else
            case getBlockBeforeKey key editor of
                Just prevBlock ->
                    let
                        nextSelection =
                            SelectionState.collapsed prevBlock.key (ContentBlock.textLength prevBlock)
                    in
                        editor
                            |> setBlocks nextBlocks
                            |> setSelection nextSelection

                -- If there is no prev block, then the first block was removed
                Nothing ->
                    if OrderedDict.size nextBlocks == 0 then
                        -- Last block was removed, so reset the block list to a single empty block
                        let
                            ( emptyBlock, nextEditor ) =
                                createBlockFromText "" editor
                        in
                            nextEditor
                                |> setBlocks (OrderedDict.singleton emptyBlock.key emptyBlock)
                    else
                        -- Select the (next) first block
                        let
                            selectedBlock =
                                OrderedDict.getAtIndex 0 nextBlocks
                                    |> Util.unwrapMaybe

                            nextSelection =
                                SelectionState.collapsed selectedBlock.key 0
                        in
                            editor
                                |> setSelection nextSelection
                                |> setBlocks nextBlocks


{-| Generating a block key updates the editor
-}
generateBlockKey : EditorState -> ( String, EditorState )
generateBlockKey editor =
    ( toString editor.id
    , { editor | id = editor.id + 1 }
    )


createBlockFromText : String -> EditorState -> ( ContentBlock, EditorState )
createBlockFromText text prevEditor =
    let
        ( blockKey, editor ) =
            generateBlockKey prevEditor

        block =
            { key = blockKey
            , text = Util.orSpace text
            }
    in
        ( block, editor )


{-| Checks if editor has any renderable content.
-}
isEmpty : EditorState -> Bool
isEmpty editor =
    OrderedDict.size editor.blocks == 1 && (ContentBlock.isEmpty <| getFirstBlock editor)


getBlock : String -> EditorState -> Maybe ContentBlock
getBlock key { blocks } =
    OrderedDict.get key blocks


{-| Editor ensures that there is always at least one block.
-}
getFirstBlock : EditorState -> ContentBlock
getFirstBlock { blocks } =
    OrderedDict.getAtIndex 0 blocks
        |> Util.unwrapMaybe


getCurrentBlock : EditorState -> ContentBlock
getCurrentBlock editor =
    let
        key =
            editor.selection.anchorKey
    in
        getBlock key editor
            -- Editor functions should always ensure there's at least one block
            |> Util.unwrapMaybe


insertBlockAfterKey : String -> ContentBlock -> EditorState -> EditorState
insertBlockAfterKey key block editor =
    { editor | blocks = OrderedDict.insertAfter key ( block.key, block ) editor.blocks }


{-| Whenever a renderable key is pressed, we insert it at the cursor
and update the selection
-}
onTextKeyPress : Keyboard.Extra.Key -> EditorState -> EditorState
onTextKeyPress key editor =
    let
        insertion =
            key
                |> Keyboard.Extra.toCode
                |> Char.fromCode
                |> (\char ->
                        if List.member Keyboard.Extra.Shift editor.pressedKeys then
                            Char.toUpper char
                        else
                            Char.toLower char
                   )
                |> String.fromChar

        block =
            getCurrentBlock editor

        nextText =
            String.Extra.insertAfterIndex editor.selection.anchorOffset insertion block.text

        nextSelection =
            SelectionState.increment editor.selection
    in
        editor
            |> updateBlock block.key (\block -> { block | text = Util.orSpace nextText })
            |> setSelection nextSelection


onBackSpace : EditorState -> EditorState
onBackSpace editor =
    if isEmpty editor then
        -- If editor has no content, then there's nothing to do.
        editor
    else if editor.selection.anchorOffset == 0 && editor.selection.anchorKey == (getFirstBlock editor).key then
        -- If we are at the start of the first block, then do nothing
        editor
    else if editor.selection.anchorOffset == 0 then
        -- If we are at the start of any non-first block, then we merge it with the block above
        let
            removedBlock =
                getCurrentBlock editor

            blockBefore =
                OrderedDict.getBefore removedBlock.key editor.blocks
                    |> Util.unwrapMaybe
        in
            editor
                |> removeBlock removedBlock.key
                |> updateBlock blockBefore.key
                    (\block ->
                        let
                            newText =
                                String.trimRight block.text
                                    ++ String.trimRight removedBlock.text
                                    |> Util.orSpace
                        in
                            { block | text = newText }
                    )
    else
        -- Delete the character before the selection
        let
            block =
                getCurrentBlock editor

            nextText =
                String.Extra.removeIndex (editor.selection.anchorOffset - 1) block.text

            prevSelection =
                editor.selection

            nextSelection =
                SelectionState.decrement editor.selection
        in
            editor
                |> setSelection nextSelection
                |> updateBlock block.key (\block -> { block | text = Util.orSpace nextText })



-- JSON


encode : EditorState -> JE.Value
encode editor =
    JE.object
        [ "blocks" => JE.array (Array.map ContentBlock.encode (OrderedDict.toArray editor.blocks))
        , "selection" => SelectionState.encode editor.selection
        ]
