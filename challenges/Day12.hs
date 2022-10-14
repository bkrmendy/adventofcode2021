module Main where

import Advent (challenge)
import Utils ()

import Data.Char (isUpper)
import Data.List.Split (splitOn)

import qualified Data.Set as S

type Graph = [(String, String)]

type Challenge = Graph

parse :: String -> Challenge
parse = concatMap edge . lines
  where edge e = let [a, b] = splitOn "-" e in [(a, b), (b, a)]
  
big :: String -> Bool
big = isUpper . head

type Seen = S.Set String

edgesFrom :: Graph -> String -> [String]
edgesFrom graph e = map snd $ filter (\(a, _) -> a == e) graph

paths :: Bool -> Graph -> [[String]]
paths initial graph = pathsI initial "start" (S.singleton "start")
  where
    pathsI :: Bool -> String -> Seen -> [[String]]
    pathsI v cave seen | cave == "end" = [["end"]]
                       | otherwise = do
                          edge <- edgesFrom graph cave
                          if not (edge `S.member` seen) || big edge
                           then (cave:) <$> pathsI v edge (edge `S.insert` seen)
                           else if not (big edge) && edge /= "start" && not v
                            then (cave:) <$> pathsI True edge seen
                            else mempty

part1 :: Challenge -> Int
part1 = length . paths True

part2 :: Challenge -> Int
part2 = length . paths False

main :: IO ()
main = challenge 12 parse part1 part2