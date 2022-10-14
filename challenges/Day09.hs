module Main where

import Advent (challenge)
import Utils (runBFS)

import            Data.List (sortOn)
import            Data.Char (digitToInt)
import            Control.Monad (guard)
import qualified  Data.Array.Unboxed as A

type Seafloor = A.UArray (Int, Int) Int
type Point = (Int, Int)

type Challenge = Seafloor

parse :: String -> Challenge
parse input = A.array ((0, 0), (rows, cols)) $ do
  (row, iRow) <- zip ls [0..]
  (tile, iCol) <- zip row [0..]
  pure ((iRow, iCol), digitToInt tile)
  where ls = lines input
        (rows, cols) = (length ls - 1, length (head ls) - 1)

neighbors :: Seafloor -> (Int, Int) -> [(Int, Int)]
neighbors m (row, col) = do
  ns <- [(row - 1, col), (row + 1, col), (row, col - 1), (row, col + 1)]
  guard $ A.inRange (A.bounds m) ns
  pure ns

riskLevel :: Int -> Int
riskLevel = (+ 1)

isLowPoint :: Seafloor -> (Int, Int) -> Bool
isLowPoint m point = all ((p <) . (m A.!)) $ neighbors m point
  where p = m A.! point

lowPoints :: Seafloor -> [(Int, Int)]
lowPoints m = [(r, c) | r <- [mir .. mar], c <- [mic .. mac], isLowPoint m (r, c)]
  where ((mir, mic), (mar, mac)) = A.bounds m

part1 :: Challenge -> Int
part1 seafloor = sum
               $ map (riskLevel . (seafloor A.!))
               $ lowPoints seafloor

basinSizeOf :: Seafloor -> (Int, Int) -> Int
basinSizeOf m start = length $ runBFS neighborsI [start] 
  where
    neighborsI p = [ns | ns <- neighbors m p, m A.! ns /= 9]

part2 :: Challenge -> Int
part2 seafloor = product
               $ take 3
               $ sortOn negate
               $ map (basinSizeOf seafloor) 
               $ lowPoints seafloor 

main :: IO ()
main = challenge 9 parse part1 part2