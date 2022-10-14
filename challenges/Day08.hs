module Main where

import Advent (challenge)
import Data.List (sort)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M

import Control.Monad (guard)

type Entry = ([String], [String])
type Challenge = [Entry]

entry :: String -> Entry
entry line = (sort <$> words code, sort <$> words ns)
  where [code, ns] = splitOn " | " line

type Mapping = M.Map String Int

select :: [a] -> [(a, [a])]
select [] = error "Cannot select from empty list"
select [x] = [(x, [])]
select (x:xs) = (x, xs):rest
  where rest = [ (a, x:as) | (a, as) <- select xs]

isSubsetOf :: (Eq a) => [a] -> [a] -> Bool
a `isSubsetOf` b = all (`elem` b) a

segments :: String -> Int -> Bool
segments a n = length a == n

mkMapping :: [String] -> Mapping
mkMapping xs = M.fromList $ zipWith (\n a -> (a, n)) [0..] $ head $ do
  (one, none) <- select xs
  guard $ segments one 2
  
  (four, nfour) <- select none
  guard $ segments four 4
  
  (seven, nseven) <- select nfour
  guard $ segments seven 3
  
  (eight, neight) <- select nseven
  guard $ segments eight 7
  
  (three, nthrees) <- select neight
  guard $ segments three 5 && one `isSubsetOf` three
  
  (zero, nzeros) <- select nthrees
  guard $ segments zero 6 && one `isSubsetOf` zero
  
  (nine, nnines) <- select nzeros
  guard $ segments nine 6 && four `isSubsetOf` nine
  
  (five, nfives) <- select nnines
  guard $ segments five 5 && five `isSubsetOf` nine
  
  (six, nsix) <- select nfives
  guard $ segments six 6
  
  (two, _) <- select nsix
  
  pure [zero, one, two, three, four, five, six, seven, eight, nine]

display :: Mapping -> [String] -> [Int]
display mapping = map (mapping M.!)

process :: Entry -> [Int]
process (code, ns) = display (mkMapping code) ns

parse :: String -> Challenge
parse = map entry . lines

part1 :: Challenge -> Int
part1 = sum . map (length . filter simple . process)
    where simple a = a `elem` [1,4,7,8]

toInt :: [Int] -> Int
toInt = foldl (\a n -> a * 10 + n) 0

part2 :: Challenge -> Int
part2 = sum . map (toInt . process)

main :: IO ()
main = challenge 8 parse part1 part2