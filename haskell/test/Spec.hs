-- Example.hs  --  Examples from HUnit user's guide
--
-- For more exampsles, check out the tests directory.  It contains unit tests
-- for HUnit.

import Test.HUnit

import Lib (makeGrid, set)

t1 :: Test
t1 = TestCase (assertEqual "spawning" [False] (makeGrid 1))

tests :: Test
tests =
  TestList
    [ TestLabel "t1" t1,
      "makeGrid 3" ~: [False, False, False, False] ~=? makeGrid 2,
      "set one to true" ~: [True] ~=? set [False] 0 True,
      "set other to true" ~: [False, True, False, False] ~=? set (makeGrid 2) 1 True,
      "set on empty list?" ~: [] ~=? set [] 0 True
    ]

main :: IO Counts
main = runTestTT tests
