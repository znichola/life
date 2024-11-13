import Lib
  ( evolve,
    get,
    getAt,
    makeGrid,
    neighbours,
    nextGeneration,
    set,
    setAt,
  )
import Test.HUnit

t1 :: Test
t1 = TestCase (assertEqual "spawning" [False] (makeGrid 1))

tests :: Test
tests =
  TestList
    [ TestLabel "t1" t1,
      "makeGrid 3" ~: [False, False, False, False] ~=? makeGrid 2,
      "set one to true" ~: [True] ~=? set [False] 0 True,
      "set other to true" ~: [False, True, False, False] ~=? set (makeGrid 2) 1 True,
      "set on empty list?" ~: [] ~=? set [] 0 True,
      "get one" ~: False ~=? get [False] 0,
      "get one from many" ~: True ~=? get [False, True, False, False] 1,
      "get on empty list" ~: False ~=? get [] 0,
      "get last one" ~: True ~=? get [False, True, False, True] 3,
      "get past end" ~: False ~=? get [True] 2,
      "get negative" ~: False ~=? get [True] (-1),
      "getAt bugged thing" ~: False ~=? getAt 1 [True] 1 (-1),
      "getAt bugged thing 2" ~: False ~=? getAt 1 [True] (-1) 1,
      "set last one" ~: [False, False, False, True] ~=? set (makeGrid 2) 3 True,
      "get at corrd" ~: False ~=? getAt 1 [False] 0 0,
      "set at corrd" ~: [True] ~=? setAt 1 [False] 0 0 True,
      "get neighbours" ~: replicate 8 False ~=? neighbours 3 (makeGrid 3) 0 0,
      "get neighbours 2" ~: True : replicate 7 False ~=? neighbours 3 (set (makeGrid 3) 0 True) 1 1,
      "get neighbours edge" ~: [False, False, False, False, True, False, False, False] ~=? neighbours 3 (set (makeGrid 3) 1 True) 0 0,
      "neighbours 0" ~: [False, False, False, False, True, False, True, True] ~=? neighbours 2 [False, True, True, True] 0 0,
      "neighbours all out of bounds" ~: replicate 8 False ~=? neighbours 1 [True] 0 0,
      "evolve one cell" ~: True ~=? evolve 2 [False, True, True, True] 0 0,
      "next generation" ~: replicate 4 True ~=? nextGeneration 2 [False, True, True, True],
      "" ~: True ~=? True
    ]

main :: IO Counts
main = runTestTT tests
