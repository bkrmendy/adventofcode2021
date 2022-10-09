module Main where

import Advent (challenge)
import Utils (readInt, threes)

parse :: String -> [Int]
parse = map readInt . lines

countIncreasing :: [Int] -> Int
countIncreasing ns = length $ filter (uncurry (<)) $ zip ns (tail ns)

part1 :: [Int] -> Int
part1 = countIncreasing

part2 :: [Int] -> Int
part2 = countIncreasing . map sum . threes

main :: IO ()
main = challenge 1 parse part1 part2