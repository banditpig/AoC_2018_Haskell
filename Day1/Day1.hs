{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-unused-do-bind #-}
import           Input
import qualified Data.Map as M

itemAt :: Int -> [Int] -> Int
itemAt ix xs = xs !! (ix `mod` length xs)

search :: Int -> Int -> M.Map Int Int -> [Int] -> Int
search ix total seenMap xs = 
    if M.member nextVal seenMap
        then nextVal
        else search (ix + 1) nextVal (M.insert nextVal nextVal seenMap) xs  
        where nextVal = nextTotal total ix xs 
   
nextTotal :: Int -> Int -> [Int] -> Int
nextTotal total ix xs 
    | ix == 0 = itemAt ix xs
    | otherwise = total + itemAt ix xs

answer1 :: [Int] -> Int
answer1 = sum

answer2 :: [Int] -> Int
answer2 = search 0 0 M.empty 

main :: IO ()
main = do 
       d <- withData "Day1.txt" parserListInt
       print $ answer1 d
       print $ answer2 d