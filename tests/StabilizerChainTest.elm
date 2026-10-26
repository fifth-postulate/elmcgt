module StabilizerChainTest exposing (suite)

import Expect
import Test exposing (Test, test, describe)
import CGT.StabilizerChain

suite : Test
suite =
    describe "StabilizerChain module"
    [
    test "harness" <| \_ ->
        Expect.equal 42 42 
        
    ] 
