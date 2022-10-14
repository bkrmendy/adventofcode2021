module Main where

import Advent (challenge)
import Utils (readInt)

import Data.List (sort)
import Data.List.Split (splitOn)

type Challenge = [Int]

type Fuel = Int -> Int -> Int

tri :: Int -> Int
tri n = n * (n + 1) `div` 2

median :: [Int] -> Int
median xs = sort xs !! (length xs `div` 2)

mean :: [Int] -> Int
mean xs = sum xs `div` length xs

fuelPt1, fuelPt2 :: Fuel
fuelPt1 from to = abs (to - from)
fuelPt2 from to = tri $ abs (to - from)

alignTo :: Fuel -> Int -> [Int] -> Int
alignTo fuel pos = sum . map (fuel pos)

parse :: String -> Challenge
parse = map readInt . splitOn ","

part1 :: Challenge -> Int
part1 xs = alignTo fuelPt1 (median xs) xs

part2 :: Challenge -> Int
part2 xs = alignTo fuelPt2 (mean xs) xs

main :: IO ()
main = challenge 7 parse part1 part2