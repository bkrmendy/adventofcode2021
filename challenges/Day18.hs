module Main where

import Advent (challenge)
import Utils (int, parseL)

import Data.List (foldl')

import Text.Parsec ((<|>))
import qualified Text.Parsec as P

-- | source: https://stackoverflow.com/a/23924238
converge :: Eq a => (a -> a) -> a -> a
converge = until =<< ((==) =<<)

data Number
  = Digit Int
  | Pair Number Number
  deriving (Eq)

type Challenge = [Number]

number, digit, pair :: P.Parsec String () Number
number = digit <|> pair
digit = Digit <$> int
pair = Pair <$> (P.string "[" *> number) <*> (P.char ',' *> number <* P.string "]")

magnitude :: Number -> Int
magnitude (Digit n) = n
magnitude (Pair left right) = (3 * magnitude left) + (2 * magnitude right)

data ExplodeResult
  = Unchanged Number
  | Explode (Maybe Int) (Maybe Int) Number

addLeft :: Int -> Number -> Number
addLeft n (Digit d)  = Digit (d + n)
addLeft n (Pair left right) = Pair (addLeft n left) right

addRight :: Int -> Number -> Number
addRight n (Digit d) = Digit (d + n)
addRight n (Pair left right) = Pair left (addRight n right)

doExplode :: Int -> Number -> ExplodeResult
doExplode _ (Digit d) = Unchanged $ Digit d
doExplode 0 (Pair (Digit left) (Digit right)) = Explode (Just left) (Just right) (Digit 0)
doExplode n (Pair left right) =
  case doExplode (n - 1) left of
    Explode dLeft Nothing       left' -> Explode dLeft Nothing (Pair left' right)
    Explode dLeft (Just dRight) left' -> Explode dLeft Nothing (Pair left' (addLeft dRight right))
    Unchanged left' ->
      case doExplode (n - 1) right of
        Explode Nothing       dRight right' -> Explode Nothing dRight (Pair left' right')
        Explode (Just dLeft)  dRight right' -> Explode Nothing dRight (Pair (addRight dLeft left') right')
        Unchanged right'                    -> Unchanged (Pair left' right')

runExplode :: Int -> Number -> Number
runExplode _ (Digit d) = Digit d
runExplode n p =
  case doExplode n p of
    Unchanged result -> result
    Explode _ _ result -> explode result

explode :: Number -> Number
explode = runExplode 4

split :: Number -> Number
split (Pair left right)
  | left /= sLeft = Pair sLeft right
  | right /= sRight = Pair left sRight
  | otherwise = Pair left right
  where sLeft = split left
        sRight = split right

split (Digit d) | d >= 10 = Pair (Digit lo) (Digit hi)
                | otherwise = Digit d
  where lo = d `div` 2
        hi = (d + 1) `div` 2

add :: Number -> Number -> Number
add left right = reduce (Pair left right)

reduce :: Number -> Number
reduce = converge (split . explode)

parse :: String -> Challenge
parse = parseL number

part1 :: Challenge -> Int
part1 [] = 0
part1 (first:rest) = magnitude $ foldl' add first rest

part2 :: Challenge -> Int
part2 = maximum . allProducts
  where allProducts ns = [magnitude result | n1 <- ns, n2 <- ns, n1 /= n2, let result = add n1 n2] 

main :: IO ()
main = challenge 18 parse part1 part2
