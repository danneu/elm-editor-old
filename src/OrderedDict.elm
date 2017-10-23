module OrderedDict
    exposing
        ( OrderedDict
        , get
        , insertBefore
        , getAtIndex
        , getKeyIndex
        , insertAfter
        , insertStart
        , insertEnd
        , remove
        , getBefore
        , getAfter
        , size
        , foldl
        , singleton
        , toArray
        , empty
        , fromList
        , update
        , toList
        , keys
        , items
        )

{-| Implements a dictionary with ordered keys and functions
for inserting items before and after existing keys.
-}

import Array exposing (Array)
import Array.Extra
import Dict exposing (Dict)
import Util


type alias State comparable a =
    { items : Dict comparable a

    -- Tracks ordering of the keys in the dict
    , keys : Array comparable
    }


type OrderedDict comparable a
    = OrderedDict (State comparable a)


update : comparable -> (v -> v) -> OrderedDict comparable v -> OrderedDict comparable v
update key update ((OrderedDict { items, keys }) as odict) =
    case get key odict of
        Nothing ->
            odict

        Just value ->
            let
                nextItems =
                    Dict.insert key (update value) items
            in
                OrderedDict (State nextItems keys)


empty : OrderedDict comparable v
empty =
    OrderedDict (State Dict.empty Array.empty)


singleton : comparable -> v -> OrderedDict comparable v
singleton k v =
    OrderedDict (State (Dict.singleton k v) (Array.fromList [ k ]))


fromList : List ( comparable, v ) -> OrderedDict comparable v
fromList pairs =
    List.foldl (\pair odict -> insertEnd pair odict) empty pairs


size : OrderedDict comparable a -> Int
size (OrderedDict { keys }) =
    Array.length keys


items : OrderedDict comparable a -> Dict comparable a
items (OrderedDict { items }) =
    items


keys : OrderedDict comparable a -> Array comparable
keys (OrderedDict { keys }) =
    keys


getKeyIndex : comparable -> OrderedDict comparable v -> Maybe Int
getKeyIndex key ((OrderedDict { keys }) as odict) =
    get key odict
        |> Maybe.andThen (\_ -> Array.Extra.findFirstIndex (\k -> k == key) keys)


getAtIndex : Int -> OrderedDict comparable v -> Maybe v
getAtIndex index (OrderedDict { items, keys }) =
    keys
        |> Array.get index
        |> Maybe.andThen (\key -> Dict.get key items)


foldl : (a -> b -> b) -> b -> OrderedDict comparable a -> b
foldl step init odict =
    odict
        |> toArray
        |> Array.foldl step init


get : comparable -> OrderedDict comparable a -> Maybe a
get key (OrderedDict { keys, items }) =
    Dict.get key items


getKeyAfter : comparable -> OrderedDict comparable a -> Maybe comparable
getKeyAfter key ((OrderedDict { keys, items }) as odict) =
    get key odict
        |> Maybe.andThen (\_ -> Array.Extra.findFirstIndex (\k -> k == key) keys)
        |> Maybe.andThen (\index -> Array.get (index + 1) keys)


getAfter : comparable -> OrderedDict comparable a -> Maybe a
getAfter key ((OrderedDict { keys, items }) as odict) =
    getKeyAfter key odict
        |> Maybe.andThen (\after -> Dict.get after items)


getKeyBefore : comparable -> OrderedDict comparable a -> Maybe comparable
getKeyBefore key ((OrderedDict { keys, items }) as odict) =
    get key odict
        |> Maybe.andThen (\_ -> Array.Extra.findFirstIndex (\k -> k == key) keys)
        |> Maybe.andThen (\index -> Array.get (index - 1) keys)


getBefore : comparable -> OrderedDict comparable a -> Maybe a
getBefore key ((OrderedDict { keys, items }) as odict) =
    getKeyBefore key odict
        |> Maybe.andThen (\before -> Dict.get before items)


remove : comparable -> OrderedDict comparable a -> OrderedDict comparable a
remove key ((OrderedDict { items, keys }) as odict) =
    case Dict.get key items of
        Nothing ->
            odict

        Just _ ->
            OrderedDict <|
                State
                    (Dict.remove key items)
                    (Array.filter (\k -> k /= key) keys)


{-| If insertion key already exists, then it is updated and moved.
-}
insertBefore : comparable -> ( comparable, a ) -> OrderedDict comparable a -> OrderedDict comparable a
insertBefore marker ( key, value ) ((OrderedDict { items, keys }) as odict) =
    let
        nextItems =
            Dict.insert key value items

        nextKeys =
            keys
                |> Array.filter (\k -> k /= key)
                |> Array.Extra.insertBeforeMatch (\k -> k == marker) key
    in
        OrderedDict (State nextItems nextKeys)


{-| If insertion key already exists, then it is updated and moved.
-}
insertAfter : comparable -> ( comparable, a ) -> OrderedDict comparable a -> OrderedDict comparable a
insertAfter marker ( key, value ) ((OrderedDict { items, keys }) as odict) =
    let
        nextItems =
            Dict.insert key value items

        nextKeys =
            keys
                |> Array.filter (\k -> k /= key)
                |> Array.Extra.insertAfterMatch (\k -> k == marker) key
    in
        OrderedDict (State nextItems nextKeys)


insertStart : ( comparable, a ) -> OrderedDict comparable a -> OrderedDict comparable a
insertStart ( key, value ) ((OrderedDict { items, keys }) as odict) =
    let
        nextItems =
            Dict.insert key value items

        nextKeys =
            keys
                |> Array.filter (\k -> k /= key)
                |> Array.append (Array.fromList [ key ])
    in
        OrderedDict (State nextItems nextKeys)


insertEnd : ( comparable, a ) -> OrderedDict comparable a -> OrderedDict comparable a
insertEnd ( key, value ) ((OrderedDict { items, keys }) as odict) =
    let
        nextItems =
            Dict.insert key value items

        nextKeys =
            keys
                |> Array.filter (\k -> k /= key)
                |> Array.push key
    in
        OrderedDict (State nextItems nextKeys)


toArray : OrderedDict comparable a -> Array a
toArray (OrderedDict { items, keys }) =
    Array.map
        (\k ->
            Util.unwrapMaybe (Dict.get k items)
        )
        keys


toList : OrderedDict comparable a -> List a
toList =
    Array.toList << toArray
