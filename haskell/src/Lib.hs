module Lib
  ( makeGrid,
    someFunc,
    set,
    get,
    getAt,
    setAt,
    neighbours,
  )
where

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
getAt s bs x y = get bs (x + y * s)

setAt :: Int -> [] Bool -> Int -> Int -> Bool -> [] Bool
setAt s bs x y = set bs (x + y * s)

neighbours :: Int -> [] Bool -> Int -> Int -> []Bool
neighbours s bs x y =
  g (-1) (-1) : g 0 (-1) : g 1 (-1) :
  g (-1) 0 : g 1 0 :
  g (-1) 1 : g 0 1 : g 1 1 :
  [] where g x' y' = getAt s bs (x + x') (y + y')
