module CGT.StabilizerChain exposing (Chain, empty)

import CGT.Permutation as Permutation exposing (Permutation)
import Dict exposing (Dict)

type Chain
    = Empty
    | Link Stage Chain

empty: Chain
empty = Empty

type alias Stage =
    { base : Int
    , generators : List Permutation
    , transversal : Transversal
    }
    
type alias Transversal =
    Dict Int Permutation

freshStage : Int -> Int -> Stage
freshStage degree base =
    { base = base
    , generators = []
    , transversal = Dict.singleton base (Permutation.identity degree)
    }

closeOrbit : List Permutation -> Transversal -> List Int -> Transversal
closeOrbit generators transversal queue =
    case queue of
        [] -> transversal

        beta :: rest ->
            case Dict.get beta transversal of
                Nothing ->
                    closeOrbit generators transversal rest

                Just u ->
                    let
                        step : Permutation -> (Transversal, List Int) -> (Transversal, List Int)
                        step g (t, fresh) =
                            let
                                gamma = Permutation.image g beta
                            in
                            if Dict.member gamma t then
                                (t, fresh)
                            else
                                ( Dict.insert gamma (Permutation.multiply u g) t
                                , gamma :: fresh)

                        (transversal_, fresh_) =
                            List.foldl step (transversal, []) generators
                    in
                    closeOrbit generators transversal_ (rest ++ (List.reverse fresh_))
