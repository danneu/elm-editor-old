module Array.Extra exposing (..)

{-| Junk drawer of various utilities probably not
used anymore.
-}

import Array exposing (Array)
import Util


{-| Inserts value after the given index
-}
insertAfter : Int -> a -> Array a -> Array a
insertAfter index value array =
    let
        left =
            array
                |> Array.slice 0 (index + 1)
                |> Array.push value

        right =
            array
                |> Array.slice (index + 1) (Array.length array)
    in
        Array.append left right


{-| Inserts value before the given index
-}
insertBefore : Int -> a -> Array a -> Array a
insertBefore index value array =
    let
        left =
            array
                |> Array.slice 0 index
                |> Array.push value

        right =
            array
                |> Array.slice index (Array.length array)
    in
        Array.append left right


insertBeforeMatch : (a -> Bool) -> a -> Array a -> Array a
insertBeforeMatch predicate value array =
    let
        step newArray queue =
            case Array.get 0 queue of
                Nothing ->
                    newArray

                Just item ->
                    let
                        nextQueue =
                            (Array.slice 1 (Array.length queue) queue)
                    in
                        if predicate item then
                            step (newArray |> Array.push value |> Array.push item) nextQueue
                        else
                            step (Array.push item newArray) nextQueue
    in
        step Array.empty array


insertAfterMatch : (a -> Bool) -> a -> Array a -> Array a
insertAfterMatch predicate value array =
    let
        step newArray queue =
            case Array.get 0 queue of
                Nothing ->
                    newArray

                Just item ->
                    let
                        nextQueue =
                            (Array.slice 1 (Array.length queue) queue)
                    in
                        if predicate item then
                            step (newArray |> Array.push item |> Array.push value) nextQueue
                        else
                            step (Array.push item newArray) nextQueue
    in
        step Array.empty array


{-| Remove item at given index
-}
removeIndex : Int -> Array a -> Array a
removeIndex index array =
    let
        left =
            array
                |> Array.slice 0 index

        right =
            array
                |> Array.slice (index + 1) (Array.length array)
    in
        Array.append left right


findAndRemove : (a -> Bool) -> Array a -> Array a
findAndRemove predicate array =
    let
        length =
            Array.length array

        step index =
            if index >= length then
                array
            else
                let
                    currItem =
                        Array.get index array
                            |> Util.unwrapMaybe
                in
                    if predicate currItem then
                        removeIndex index array
                    else
                        step (index + 1)
    in
        step 0


findFirstIndex : (a -> Bool) -> Array a -> Maybe Int
findFirstIndex predicate array =
    let
        length =
            Array.length array

        step index =
            if index >= length then
                Nothing
            else
                let
                    currItem =
                        Array.get index array
                            |> Util.unwrapMaybe
                in
                    if predicate currItem then
                        Just index
                    else
                        step (index + 1)
    in
        step 0


findFirst : (a -> Bool) -> Array a -> Maybe a
findFirst predicate array =
    findFirstIndex predicate array
        |> Maybe.andThen (\index -> Array.get index array)


{-| Short-circuits with True if any items of array satisfy predicate.
-}
some : (a -> Bool) -> Array a -> Bool
some predicate array =
    let
        length =
            Array.length array

        step index =
            if index >= length then
                False
            else
                let
                    currItem =
                        Array.get index array
                            |> Util.unwrapMaybe
                in
                    if predicate currItem then
                        True
                    else
                        step (index + 1)
    in
        step 0
