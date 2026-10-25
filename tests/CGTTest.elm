module CGTTest exposing (suite)

import Expect
import Test exposing (Test, test)
import CGT

suite : Test
suite =
    test "The answer to Life, the universe and everything" <| \_ ->
        Expect.equal CGT.answer 42 
