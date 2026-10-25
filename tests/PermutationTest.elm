module PermutationTest exposing (suite, permutation, permutationOfDegree)

import Expect
import Array exposing (Array)
import Fuzz exposing (Fuzzer)
import Test exposing (Test, test, describe, fuzz)
import CGT.Permutation as Permutation exposing (Permutation)

suite : Test
suite =
    describe "Permutation module"
        [ test "multiplication of two permutations" <| \_ ->
            let
                degree = 5
                
                a = Permutation.transposition degree 0 1

                b = Permutation.transposition degree 1 2
                            
                actual = Permutation.multiply a b

                expected = Permutation.fromImages [2, 0, 1, 3, 4]

            in
            Expect.equal expected actual 
        , fuzz (permutationOfDegree 7) "inverse produces a left inverse" <| \p ->
            let
                inverse = Permutation.inverse p

                answer = Permutation.multiply inverse p
            in
            Expect.equal True (Permutation.isIdentity answer)
        , fuzz (permutationOfDegree 7) "inverse produces a right inverse" <| \p ->
            let
                inverse = Permutation.inverse p

                answer = Permutation.multiply p inverse
            in
            Expect.equal True (Permutation.isIdentity answer)
        , fuzz (permutationOfDegree 12) "inverse is an involution" <| \original ->
            let

                result =
                    original
                        |> Permutation.inverse
                        |> Permutation.inverse
            in
            Expect.equal original result
        ]


{-| A fuzzer for permutations of exactly degree `n`. Shrinks toward the identity. -}
permutationOfDegree : Int -> Fuzzer Permutation
permutationOfDegree n =
    Fuzz.sequence (List.repeat n (Fuzz.intRange 0 (max 0 (n - 1))))
        |> Fuzz.map (fromChoices n)


{-| A fuzzer for permutations of a random degree between 1 and `maxDegree`. -}
permutation : Int -> Fuzzer Permutation
permutation maxDegree =
    Fuzz.intRange 1 maxDegree
        |> Fuzz.andThen permutationOfDegree


fromChoices : Int -> List Int -> Permutation
fromChoices n choices =
    let
        step ( i, choice ) arr =
            let
                j =
                    i + modBy (n - i) choice
            in
            swap i j arr

        images =
            List.indexedMap Tuple.pair choices
                |> List.foldl step (Array.initialize n identity)
    in
    Permutation.fromArray images


swap : Int -> Int -> Array Int -> Array Int
swap i j arr =
    case ( Array.get i arr, Array.get j arr ) of
        ( Just a, Just b ) ->
            arr |> Array.set i b |> Array.set j a

        _ ->
            arr
