module Main where

import Prelude hiding (round)
import Advent (challenge)
import Data.Char (digitToInt)
import Data.List (foldl')
import Control.Monad.State

import qualified Data.Map.Strict as M

data Player = MkPlayer { _position :: Int, _score :: Int } deriving (Eq, Ord, Show)

type Challenge = (Player, Player)

parse :: String -> Challenge
parse input = (MkPlayer (startingPosition player1) 0, MkPlayer (startingPosition player2) 0)
  where [player1, player2] = lines input
        startingPosition = digitToInt . last

type Die = [Int]
deterministic, dirac :: Die
deterministic = [1..]
dirac = [i + j + k | i <- [1, 2, 3], j <- [1, 2, 3], k <- [1, 2, 3]]

move :: Player -> Int -> Player
move (MkPlayer pos score) delta = MkPlayer pos' (score + pos')
  where pos' = ((pos + delta - 1) `mod` 10) + 1

playPt1 :: Int -> [Int] -> Player -> Player -> Int
playPt1 round (r1:r2:r3:rest) p1 p2
  | _score p1 >= 1000 = 3 * round * _score p2
  | _score p2 >= 1000 = 3 * round * _score p1
  | otherwise         = playPt1 (round + 1) rest p2 (move p1 (r1 + r2 + r3))

tupleSum :: [(Int, Int)] -> (Int, Int)
tupleSum = foldl' (\(la, ra) (l, r) -> (la + r, ra + l)) (0, 0)

type WinCache = M.Map (Player, Player) (Int, Int)

playUniverse :: (Player, Player) -> State WinCache (Int, Int)
playUniverse (p1, p2)
  | _score p1 >= 21 = return (1, 0)
  | _score p2 >= 21 = return (0, 1)
  | otherwise = do
    cache <- get
    case M.lookup (p1, p2) cache of
      Just res -> return res
      Nothing -> do
        results <- tupleSum <$> forM dirac (\roll -> playUniverse (p2, move p1 roll))
        modify' $ M.insert (p1, p2) results
        return results

part1 :: Challenge -> Int
part1 (p1, p2) = playPt1 0 deterministic p1 p2

part2 :: Challenge -> Int
part2 = uncurry max . flip evalState M.empty . playUniverse

main :: IO ()
main = challenge 21 parse part1 part2