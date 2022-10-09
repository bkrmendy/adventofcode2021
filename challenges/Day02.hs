module Main where

import Advent (challenge)
import Utils (readInt)
import Data.List (foldl')

data Direction
  = Forward Int
  | Down Int
  | Up Int

type Challenge = [Direction]

direction :: String -> Direction
direction dir = case words dir of
  ["forward", n] -> Forward (readInt n)
  ["down", n] -> Down (readInt n)
  ["up", n] -> Up (readInt n)
  _ -> error $ "Cannot parse direction: " <> dir

parse :: String -> Challenge
parse = map direction . lines

data Sub = Sub { _aim :: Int, _depth :: Int, _horizontal :: Int }

result :: Sub -> Int
result sub = _depth sub * _horizontal sub

part1 :: Challenge -> Int
part1 = result . foldl' move (Sub 0 0 0)
  where
    move :: Sub -> Direction -> Sub
    move (Sub a d h) (Forward n) = Sub a d (h + n)
    move (Sub a d h) (Up n) = Sub a (d - n) h
    move (Sub a d h) (Down n) = Sub a (d + n) h

part2 :: Challenge -> Int
part2 = result . foldl' move (Sub 0 0 0)
  where
    move :: Sub -> Direction -> Sub
    move (Sub a d h) (Forward n) = Sub a (d + (a * n)) (h + n)
    move (Sub a d h) (Up n) = Sub (a - n) d h
    move (Sub a d h) (Down n) = Sub (a + n) d h

main :: IO ()
main = challenge 2 parse part1 part2