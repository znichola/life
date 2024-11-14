module Lib
  ( makeGrid,
    someFunc,
    set,
    get,
    getAt,
    setAt,
    neighbours,
    evolve,
    nextGeneration,
    getX,
    getY,
  )
where

import GHC.Utils.Misc (count)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

makeGrid :: Int -> [] Bool
makeGrid n = replicate (n * n) False

set :: [] Bool -> Int -> Bool -> [] Bool
set [] _ _ = []
set (_ : xs) 0 val = val : xs
set (x : xs) n val = x : set xs (n - 1) val

get :: [] Bool -> Int -> Bool
get [] _ = False
get (z : _) 0 = z
get (_ : zs) n = get zs (n - 1)

getAt :: Int -> [] Bool -> Int -> Int -> Bool
getAt _ _ x y
  | x < 0 || y < 0 = False
getAt s bs x y = get bs (x + y * s)

setAt :: Int -> [] Bool -> Int -> Int -> Bool -> [] Bool
setAt s bs x y = set bs (x + y * s)

neighbours :: Int -> [] Bool -> Int -> Int -> [] Bool
neighbours s bs x y =
  [g (-1) (-1), g 0 (-1), g 1 (-1), g (-1) 0, g 1 0, g (-1) 1, g 0 1, g 1 1]
  where
    g x' y' = getAt s bs (x + x') (y + y')

evolve :: Int -> [] Bool -> Int -> Int -> Bool
evolve s bs x y = if c == 2 then getAt s bs x y else c == 3
  where
    c = count id (neighbours s bs x y)

getX :: Int -> Int -> Int
getX 0 _ = 0
getX s i = mod i s

-- 0 1 2
-- 3 4 5
-- 6 7 8

getY :: Int -> Int -> Int
getY 0 _ = 0
getY s i = div i s

nextGeneration :: Int -> [] Bool -> [] Bool
nextGeneration s bs = map go [0 .. (length bs - 1)]
  where
    go i = evolve s bs (getX s i) (getY s i)
