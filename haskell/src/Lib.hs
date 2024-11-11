module Lib
  ( makeGrid,
    someFunc,
    set,
  )
where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

makeGrid :: Int -> [] Bool
makeGrid n = replicate (n * n) False

set :: []Bool -> Int -> Bool -> []Bool
set [] _ _ = []
set (_:xs) 0 val = val : xs
set (x:xs) n val = x : set xs (n - 1) val
