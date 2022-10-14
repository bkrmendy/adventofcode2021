module Main where

import Advent (challenge)
import Utils (readInt)

import Data.List.Split (splitOn)
import Data.Array.ST
import Data.Array.Unboxed

import Control.Monad (forM_)

type School = [Int]
type Challenge = School

parse :: String -> School
parse ns = [ count n school | n <- [0..8]]
 where school = map readInt . splitOn "," $ ns
       count x = length . filter (== x)

-- | based on https://github.com/encse/adventofcode/blob/master/2021/Day06/Solution.cs    
runLanternFish :: Int -> School -> Int
runLanternFish days school = sum $ elems $ runSTUArray $ do
  fish <- newListArray (0, 8) school
  forM_ [0 .. days - 1] $ \day -> do
    elemA <- readArray fish (day `mod` 9)
    elemB <- readArray fish ((day + 7) `mod` 9)
    writeArray fish ((day + 7) `mod` 9) (elemA + elemB)
  return fish

part1 :: Challenge -> Int
part1 = runLanternFish 80

part2 :: Challenge -> Int
part2 = runLanternFish 256

main :: IO ()
main = challenge 6 parse part1 part2