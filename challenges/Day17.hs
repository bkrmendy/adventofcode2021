module Main where

import Advent (challenge)
import Utils ()

data Bounds = MkBounds (Int, Int) (Int, Int)

type Challenge = Bounds

parse :: String -> Challenge
parse = const $ MkBounds (137, 171) (-98, -73)

data Probe = MkProbe { _position :: (Int, Int), _velocity :: (Int, Int) }

inBounds :: Bounds -> Probe -> Bool
inBounds (MkBounds (minX, maxX) (minY, maxY)) (MkProbe (px, py) _) = px <= maxX && px >= minX && py >= minY && py <= maxY

sling :: Bounds -> Probe -> Maybe [Probe]
sling b@(MkBounds (_, maxX) (minY, _)) probe@(MkProbe (px, py) (vx, vy))
  | px > maxX || py < minY = Nothing
  | inBounds b probe = Just [probe]
  | otherwise = (nextProbe:) <$> sling b nextProbe 
  where nextProbe = MkProbe (px + vx, py + vy) (max 0 (vx - 1), vy - 1)

run :: Bounds -> [Int]
run b@(MkBounds (_, maxX) (minY, _)) = do
  vx <- [0..maxX]
  vy <- [minY .. -minY]
  case sling b (MkProbe (0, 0) (vx, vy)) of
    Nothing -> mempty
    Just ps -> pure $ maximum $ map (snd . _position) ps

part1 :: Challenge -> Int
part1 = maximum . run

part2 :: Challenge -> Int
part2 = length . run

main :: IO ()
main = challenge 17 parse part1 part2