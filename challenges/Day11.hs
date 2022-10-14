module Main where

import Advent (challenge)
import Utils (countWhere)

import Data.Char (digitToInt)
import Control.Monad (guard)

import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import qualified Data.Array.Unboxed as A

type Map = A.UArray (Int, Int) Int

type Challenge = Map

parse :: String -> Challenge
parse ls = A.array ((0, 0), (9, 9)) $ do
      (line, row) <- zip (lines ls) [0..]
      (column, col) <- zip line [0..]
      pure ((row, col), digitToInt column)

neighbors :: Map -> (Int, Int) -> [(Int, Int)]
neighbors m (r, c) = do
  n <- [(r + rd, c + cd) | rd <- [-1..1], cd <- [-1..1] ]
  guard $ A.inRange (A.bounds m) n && n /= (r, c)
  pure n

boostNeighbors :: Map -> (Int, Int) -> Map
boostNeighbors m coord = m A.// ns
  where ns = [ (n, m A.! n + 1) | n <- neighbors m coord ]

flashedNeighbors :: Map -> (Int, Int) -> [(Int, Int)]
flashedNeighbors m coord = [ n | n <- neighbors m coord, m A.! n > 9 ]

pump, damp, flash :: Map -> Map
pump = A.amap (+ 1)
damp = A.amap (\a -> if a > 9 then 0 else a)

flash m = runFlash m (Seq.fromList [coord | (coord, value) <- A.assocs m, value > 9]) Set.empty
  where
    runFlash :: Map -> Seq.Seq (Int, Int) -> Set.Set (Int, Int) -> Map
    runFlash m' queue seen = case Seq.viewl queue of
      Seq.EmptyL -> m'
      coord Seq.:< rest -> if Set.member coord seen
        then runFlash m' rest seen
        else
          let
            m'' = boostNeighbors m' coord
            ns = flashedNeighbors m'' coord
          in
            runFlash m'' (rest Seq.>< Seq.fromList ns) (Set.insert coord seen)

step :: Map -> Map
step = damp . flash . pump

flashed :: Map -> Int
flashed = countWhere (== 0) . A.elems

maps :: Map -> [Map]
maps m = let next = step m in next:maps next

part1 :: Challenge -> Int
part1 = sum . map flashed . take 100 . maps

part2 :: Challenge -> Int
part2 m = runI (maps m) 1
  where runI (ma:mas) t | flashed ma == 100 = t
                        | otherwise = runI mas (t + 1)

main :: IO ()
main = challenge 11 parse part1 part2