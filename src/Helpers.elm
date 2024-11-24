module Helpers exposing (..)

import Array


arrayPrepend : Array.Array a -> Array.Array a -> Array.Array a
arrayPrepend =
    Array.append


countOccurrences : Array.Array String -> String -> Int
countOccurrences chars string =
    string
        |> String.split ""
        |> List.filter (\char -> arrayMember char chars)
        |> List.length


modBy : Int -> Int -> Int
modBy =
    Basics.modBy


round : Float -> Int
round =
    Basics.round


stringJoin : String -> Array.Array String -> String
stringJoin string array =
    String.join string (Array.toList array)


stringSplit : String -> String -> Array.Array String
stringSplit char string =
    Array.fromList (String.split char string)


arrayFromList : List a -> Array.Array a
arrayFromList =
    Array.fromList


arrayMember : a -> (Array.Array a -> Bool)
arrayMember a =
    Array.foldr (\i res -> a == i || res) False


arrayRange : Int -> Int -> Array.Array Int
arrayRange int1 int2 =
    Array.fromList (List.range int1 int2)


arrayPushLast : a -> Array.Array a -> Array.Array a
arrayPushLast =
    Array.push


arrayIndexedMap : (Int -> a -> b) -> Array.Array a -> Array.Array b
arrayIndexedMap f array =
    let
        mapHelper index acc =
            case Array.get index array of
                Nothing ->
                    acc

                Just value ->
                    mapHelper (index + 1) (Array.push (f index value) acc)
    in
    mapHelper 0 Array.empty


indexedFoldl : (Int -> a -> b -> b) -> b -> List a -> b
indexedFoldl func acc list =
    let
        step : a -> ( Int, b ) -> ( Int, b )
        step x ( i, thisAcc ) =
            ( i + 1, func i x thisAcc )
    in
    Tuple.second (List.foldl step ( 0, acc ) list)


arrayIndexedFoldl : (Int -> a -> b -> b) -> b -> Array.Array a -> b
arrayIndexedFoldl func acc array =
    array
        |> Array.toList
        |> indexedFoldl func acc
