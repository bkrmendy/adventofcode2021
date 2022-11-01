module Main where

import Advent (challenge)

import Prelude hiding (round)

import qualified Data.Array as A
import           Data.Array ((!))

import qualified Data.Map.Strict as M
import           Data.List.Split (splitOn)
import           Data.List (foldl')

type Coordinate = (Int, Int)
type PixelWithCoordinate = (Coordinate, Int)
type Algorithm = A.Array Int Int
type Image = M.Map Coordinate Int

data Challenge = MkChallenge {
    _algorithm :: Algorithm
  , _image :: Image
}

parse :: String -> Challenge
parse input = MkChallenge (parseAlgorithm algorithm) (parseImage image)
  where [algorithm, image] = "\n\n" `splitOn` input

pixel :: Char -> Int
pixel c = if c == '#' then 1 else 0

toBinary :: [Int] -> Int
toBinary = foldl' (\a p -> 2 * a + p) 0

parseAlgorithm :: String -> Algorithm
parseAlgorithm a = A.listArray (0, length a) (map pixel a)

parseImage :: String -> Image
parseImage source = M.fromList $ do
  (nRow, line) <- zip [0..] $ lines source
  (nCol, dot) <- zip [0..] line
  pure ((nRow, nCol), pixel dot)

coordsWithBounds :: Image -> [Coordinate]
coordsWithBounds image = [(r, c) | r <- [rMin - 1 .. rMax + 1], c <- [cMin - 1 .. cMax + 1]]
  where (rMin, cMin) = fst $ M.findMin image
        (rMax, cMax) = fst $ M.findMax image

enhancePixel :: Int -> Algorithm -> Image -> Coordinate -> PixelWithCoordinate
enhancePixel defaultPixel algorithm image coord = (coord, algorithm ! index)
  where index = toBinary $ neighborhood coord
        neighborhood (r, c) = [M.findWithDefault defaultPixel (r', c') image | r' <- [r-1..r+1], c' <- [c-1..c+1]]

enhance :: Int -> Algorithm -> Image -> Image
enhance 0 _ image = image
enhance round algorithm image = enhance (round - 1) algorithm nextImage
  where defaultPixel = round `mod` 2
        nextImage = M.fromList $ map (enhancePixel defaultPixel algorithm image) $ coordsWithBounds image

numberOfPixelsLit :: Image -> Int
numberOfPixelsLit = sum . M.elems

part1 :: Challenge -> Int
part1 (MkChallenge algorithm image) = numberOfPixelsLit $ enhance 2 algorithm image

part2 :: Challenge -> Int
part2 (MkChallenge algorithm image) = numberOfPixelsLit $ enhance 50 algorithm image

main :: IO ()
main = challenge 20 parse part1 part2