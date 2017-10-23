port module Main exposing (..)

import Util
import Html exposing (Html)
import Html.Keyed
import String.Extra
import Html.Attributes
import OrderedDict
import Keyboard.Extra exposing (Key(..), Msg(..), KeyChange(..))
import ContentBlock exposing (ContentBlock)
import Json.Encode
import SelectionState exposing (SelectionState)
import EditorState exposing (EditorState)


-- PORTS TO JAVASCRIPT


port renderSelection : SelectionState -> Cmd msg



-- PORTS FROM JAVASCRIPT


port selectionChange : (SelectionState -> msg) -> Sub msg



-- MODEL


type alias Model =
    EditorState


init : ( Model, Cmd Msg )
init =
    let
        initEditor =
            EditorState.empty
    in
        ( initEditor
        , renderSelection initEditor.selection
        )



-- UPDATE


type Msg
    = NoOp
    | SelectionChange SelectionState
    | KeyMsg Keyboard.Extra.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SelectionChange selection ->
            ( { model | selection = selection }, Cmd.none )

        KeyMsg keyMsg ->
            let
                ( nextPressedKeys, maybeKeyChange ) =
                    Keyboard.Extra.updateWithKeyChange keyMsg model.pressedKeys

                model2 =
                    { model | pressedKeys = nextPressedKeys }
            in
                case maybeKeyChange of
                    Nothing ->
                        ( model2
                        , Cmd.none
                        )

                    Just keyChange ->
                        case keyChange of
                            KeyDown Enter ->
                                let
                                    prevBlock =
                                        EditorState.getCurrentBlock model2

                                    ( preSplitText, postSplitText ) =
                                        String.Extra.splitAtIndex model2.selection.anchorOffset prevBlock.text

                                    ( newBlock, editor ) =
                                        EditorState.createBlockFromText (Util.orSpace postSplitText) model2

                                    selection =
                                        SelectionState.collapsed newBlock.key 0
                                in
                                    ( editor
                                        |> EditorState.updateBlock prevBlock.key (\block -> { block | text = Util.orSpace preSplitText })
                                        |> EditorState.insertBlockAfterKey (EditorState.getCurrentBlock editor).key newBlock
                                        |> EditorState.setSelection selection
                                    , renderSelection selection
                                    )

                            KeyDown BackSpace ->
                                let
                                    nextEditor =
                                        EditorState.onBackSpace model2
                                in
                                    ( nextEditor
                                    , renderSelection nextEditor.selection
                                    )

                            KeyDown key ->
                                let
                                    keyCode =
                                        Keyboard.Extra.toCode key
                                in
                                    if isTextKeyCode keyCode then
                                        let
                                            nextEditor =
                                                EditorState.onTextKeyPress key model2
                                        in
                                            ( nextEditor
                                            , renderSelection nextEditor.selection
                                            )
                                    else
                                        ( model2
                                        , Cmd.none
                                        )

                            _ ->
                                ( model2
                                , Cmd.none
                                )


{-| Checks if keycode should add to the contentblock's text
-}
isTextKeyCode : Int -> Bool
isTextKeyCode code =
    -- Numbers
    (code > 47 && code < 58)
        || -- Spacebar
           (code == 32)
        || -- Letters
           (code > 64 && code < 91)
        || -- Numpad
           (code > 185 && code < 112)
        || -- ;=,-./` (in order)
           (code > 185 && code < 193)
        || -- [\]' (in order)
           (code > 218 && code < 223)



-- VIEW


renderBlock : ContentBlock -> ( String, Html Msg )
renderBlock block =
    let
        html =
            Html.div
                [ Html.Attributes.class "block"
                , Html.Attributes.id block.key
                ]
                [ Html.text block.text ]
    in
        ( block.key, html )


renderBlocks : Model -> Html Msg
renderBlocks model =
    Html.Keyed.node
        "div"
        []
        (List.map renderBlock <| OrderedDict.toList model.blocks)


view : Model -> Html Msg
view model =
    Html.div
        []
        [ Html.div
            [ Html.Attributes.contenteditable True
            , Html.Attributes.class "editor"
            ]
            [ renderBlocks model ]
        , Html.pre
            []
            [ Html.text <| Json.Encode.encode 2 (EditorState.encode model) ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map KeyMsg Keyboard.Extra.subscriptions
        , selectionChange SelectionChange
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
