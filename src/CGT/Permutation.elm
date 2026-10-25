module CGT.Permutation exposing (Permutation, identity, transposition, multiply, fromImages, inverse, isIdentity, firstMoved, fromArray)

import Array exposing (Array)

type Permutation =
    Image Int (Array Int)

identity : Int -> Permutation
identity degree =
    Array.initialize degree Basics.identity
        |> fromArray

isIdentity : Permutation -> Bool
isIdentity p =
    firstMoved p == Nothing

firstMoved : Permutation -> Maybe Int
firstMoved (Image _ images) =
    let
        go : Int -> Maybe Int
        go i =
            case (Array.get i images) of
                Nothing -> Nothing

                Just j ->
                    if i /= j then
                        Just i
                    else
                        go (i+1)
    in
    go 0

fromArray : Array Int -> Permutation
fromArray arrayOfImages =
    Image (Array.length arrayOfImages) arrayOfImages

transposition : Int -> Int -> Int -> Permutation
transposition degree i j =
    Array.initialize degree Basics.identity
        |> Array.set i j
        |> Array.set j i
        |> fromArray

image : Permutation -> Int -> Int
image (Image _ images) i =
    Array.get i images
        |> Maybe.withDefault i

multiply : Permutation -> Permutation -> Permutation
multiply (Image _ p) q =
    p
        |> Array.map (image q)
        |> fromArray

fromImages : List Int -> Permutation
fromImages listOfImages =
    listOfImages
        |> Array.fromList
        |> fromArray

inverse : Permutation -> Permutation
inverse (Image _ p) =
    Array.foldl (\img (i, acc) -> (i + 1, Array.set img i acc)) (0, p) p
        |> Tuple.second
        |> fromArray
