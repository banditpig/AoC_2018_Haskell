{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-unused-do-bind #-}

import qualified Data.Map as M
import Data.List.Utils (replace)

countMatches :: M.Map Char Int  -> String -> M.Map Char Int
countMatches mp []  = mp 
countMatches mp (x:xs) = countMatches mp' xs  where 
    mp' = M.insertWith (+) x 1 mp 

threesAndTwos ::  M.Map Char Int -> (Int, Int)
threesAndTwos mp = (threes, twos) where 
    threes = if 3 `elem` M.elems mp then 1 else 0
    twos   = if 2 `elem` M.elems mp then 1 else 0
    

sumAndMult :: [(Int, Int)] -> Int
sumAndMult pairs = a * b where 
    (a, b) = foldr (\(x, y) (acx, acy) -> (x + acx, y + acy)) (0, 0) pairs


strDiff :: String -> String -> String
strDiff xs ys  = replace "_" "" $ zipWith (\ x y -> if x == y then x else '_' )  xs ys

compareCodes :: [String] -> String
compareCodes (x:xs) 
    | length resList == 1 = head resList
    | otherwise = compareCodes xs 
    where 
      resList = [ s | s <-  map (`strDiff` x) xs, length s  == 25] 

       
part1 :: [String] -> Int
part1 = sumAndMult . map (threesAndTwos . countMatches M.empty) 

part2 :: [String] -> String
part2  = compareCodes

main :: IO ()
main = 
    readFile "Day2.txt" >>= \d -> do
           print $ part1 (lines d)
           print $ part2 (lines d)
