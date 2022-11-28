{-# LANGUAGE TupleSections #-}

module Main where

import Prelude hiding (flip)
import Advent (challenge)
import Utils (parseL, int)

import Text.Parsec hiding (parse, count)
import Control.Applicative ()
import Data.Maybe (mapMaybe)
import Data.Bifunctor (second)

type Challenge = [(Flip, Bounds)]

data Bounds = MkBounds Int Int Int Int Int Int deriving (Eq, Ord)
data Flip = On | Off

parseOne :: Parsec String () (Flip, Bounds)
parseOne = do
  f <- flip
  _ <- string " x="
  (x, xx) <- bounds
  _ <- string ",y="
  (y, yy) <- bounds
  _ <- string ",z="
  (z, zz) <- bounds
  return (f, MkBounds x xx y yy z zz)
  where
    flip :: Parsec String () Flip
    flip = try (string "on" >> pure On) <|> (string "off" >> pure Off)
    bounds :: Parsec String () (Int, Int)
    bounds = (,) <$> int <*> (string ".." *> int)

volume :: Bounds -> Int
volume (MkBounds x xx y yy z zz) = (xx - x + 1) * (yy - y + 1) * (zz - z + 1)

intersect :: Bounds -> Bounds -> Maybe Bounds
intersect (MkBounds x xx y yy z zz) (MkBounds u uu v vv w ww) =
  if nx <= nxx && ny <= nyy && nz <= nzz
  then Just $ MkBounds nx nxx ny nyy nz nzz
  else Nothing
  where (nx, nxx, ny, nyy, nz, nzz) = (max x u, min xx uu, max y v, min yy vv, max z w, min zz ww)

parse :: String -> Challenge
parse = parseL parseOne

count :: [(Flip, Bounds)] -> Int
count [] = 0
count ((Off, _):rest) = count rest
count ((_, bound):rest) = count rest + volume bound - count (mapMaybe (isect . snd) rest)
  where isect b = (On,) <$> intersect bound b

small :: (Flip, Bounds) -> Bool
small (_, MkBounds x xx y yy z zz) = all (\t -> -50 <= t && t <= 50) [x, xx, y, yy, z, zz]

part1 :: Challenge -> Int
part1 = count . filter small

part2 :: Challenge -> Int
part2 = count

main :: IO ()
main = challenge 22 parse part1 part2