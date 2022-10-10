module Main where

import Advent (challenge)
import Utils (transpose)

import Data.List (foldl')  
import Data.Maybe (mapMaybe)

type Challenge = [[Bool]]
  
parseBit :: String -> [Bool]
parseBit = mapMaybe go
  where go '0' = Just False
        go '1' = Just True
        go _   = Nothing

parse :: String -> [[Bool]]
parse = map parseBit . lines

bitCount :: [Bool] -> (Int, Int)
bitCount = go (0, 0)
  where go (falses, trues) [] = (falses, trues)
        go (falses, trues) (True:bits) = go (falses, trues + 1) bits
        go (falses, trues) (False:bits) = go (falses + 1, trues) bits

mostCommon, leastCommon :: [Bool] -> Bool
mostCommon bits = falses <= trues
  where (falses, trues) = bitCount bits 
leastCommon bits = falses > trues
  where (falses, trues) = bitCount bits   

type BitCriteria = [Bool] -> Bool

powerRating :: BitCriteria -> [[Bool]] -> [Bool]
powerRating c = map c . transpose 

gamaRate, epsilonRate :: [[Bool]] -> [Bool]
gamaRate = powerRating mostCommon
epsilonRate = powerRating leastCommon

binaryToDec :: [Bool] -> Int
binaryToDec = foldl' (\acc val -> 2 * acc + if val then 1 else 0) 0

powerConsumption :: [[Bool]] -> Int
powerConsumption diagnostics = gama * epsilon
  where gama = binaryToDec $ gamaRate diagnostics
        epsilon = binaryToDec $ epsilonRate diagnostics
        
bitsInPos :: Bool -> Int -> [[Bool]] -> [[Bool]]
bitsInPos bit pos = filter go
  where go bits = (bits !! pos) == bit        
        
criteria :: BitCriteria -> Int -> [[Bool]] -> [Bool]
criteria _ _ [bit] = bit
criteria c p bits = criteria c (p + 1) ac
  where bc = c (transpose bits !! p)
        ac = bitsInPos bc p bits
       
oxygenGeneratorRating, co2ScrubberRating :: [[Bool]] -> [Bool]
oxygenGeneratorRating = criteria mostCommon 0
co2ScrubberRating = criteria leastCommon 0 
        
lifeSupportRating :: [[Bool]] -> Int
lifeSupportRating diagnostics = oxyRating * co2Rating
  where oxyRating = binaryToDec $ oxygenGeneratorRating diagnostics
        co2Rating = binaryToDec $ co2ScrubberRating diagnostics

part1 :: Challenge -> Int
part1 = powerConsumption

part2 :: Challenge -> Int
part2 = lifeSupportRating

main :: IO ()
main = challenge 3 parse part1 part2