module Modulemy (ex1, ex2, ex3, ex4) where

import Data.List (groupBy, sortBy, nub)
import Data.Ord (comparing)
import Data.Char (toLower, isSpace)

ex1 :: [Int] -> [[Int]]
ex1 [] = []
ex1 xs = groupBy sameParity xs
  where
    sameParity a b = even a == even b
 
ex2 :: Int -> [[Int]]
ex2 n
    | n < 0 = error "error"
    | n > 1800 = []
    | otherwise =
        sortBy (comparing last)
        [ [a, b, c, a+b+c]
        | (a, b) <- pairs
        , c <- [b..600]
        , isSquare (a*a + c*c)
        , isSquare (b*b + c*c)
        , a + b + c >= n
        ]
  where
    pairs =
        [ (a, b)
        | a <- [1..600]
        , b <- [a..600]
        , isSquare (a*a + b*b)
        ]

    isSquare x = let k = round (sqrt (fromIntegral x :: Double))
                 in k*k == x
    
ex3 :: [Int] -> [Int] -> [Int]
ex3 xs ys
    | length xs /= length ys = error "error"
    | 0 `elem` ys = error "error"
    | otherwise = findIndexs xs ys 0
        where
            findIndexs [] [] _ = []
            findIndexs (x:xs) (y:ys) i
              | x `mod` y == 0 = i: findIndexs xs ys (i + 1)
              | otherwise = findIndexs xs ys (i + 1)

trim :: String -> String
trim s = reverse (dropWhile isSpace (reverse (dropWhile isSpace s)))

normalize :: String -> String
normalize = map toLower . trim

solve :: [String] -> [String]
solve xs = nub [ y | y <- ys, count y ys > 1 ]
  where
    ys = map normalize xs
    count y = length . filter (== y)

ex4 :: FilePath -> FilePath -> IO ()
ex4 inp out = do
    contents <- readFile inp
    writeFile out (unlines (solve (lines contents)))