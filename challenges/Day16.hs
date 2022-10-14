module Main where

import Advent (challenge)
import Utils (parseLines)

import Text.Parsec as P

unsafeFromRight :: Either a b -> b
unsafeFromRight e = case e of
  Right r -> r

decode :: Char -> String
decode '0' = "0000"
decode '1' = "0001"
decode '2' = "0010"
decode '3' = "0011"
decode '4' = "0100"
decode '5' = "0101"
decode '6' = "0110"
decode '7' = "0111"
decode '8' = "1000"
decode '9' = "1001"
decode 'A' = "1010"
decode 'B' = "1011"
decode 'C' = "1100"
decode 'D' = "1101"
decode 'E' = "1110"
decode 'F' = "1111"

bitToInt :: Char -> Int
bitToInt '0' = 0
bitToInt '1' = 1

fromBinary :: String -> Int
fromBinary = foldl (\a c -> a * 2 + bitToInt c) 0

data Packet
  = Literal { _version :: Int, _typeId :: Int, _literal :: Int }
  | Operator { _version :: Int, _typeId :: Int, _packets :: [Packet] }

type Challenge = Packet

bit :: Parsec String () Char
bit = char '0' <|> char '1'

chunk :: Parsec String () String
chunk = do
  b <- bit
  bs <- count 4 bit
  case b of
    '0' -> return bs
    '1' -> do
      bbs <- chunk
      return $ bs ++ bbs

literal, operator :: Int -> Int -> Parsec String () Packet
literal vId tId = do
  number <- fromBinary <$> chunk
  return $ Literal vId tId number

operator vId tId = do
  lengthTypeId <- bit
  subPackets <- case lengthTypeId of
    '1' -> do
      nSubpackets <- fromBinary <$> count 11 bit
      count nSubpackets packet
    '0' -> do
      totalLengthBits <- fromBinary <$> count 15 bit
      packetBits <- count totalLengthBits bit
      return $ unsafeFromRight $ P.parse (many1 packet) "" packetBits
  return $ Operator vId tId subPackets


packet :: Parsec String () Packet
packet = do
  v <- fromBinary <$> count 3 bit
  t <- fromBinary <$> count 3 bit
  case t of
    4 -> literal v t
    _ -> operator v t

parsePacket :: String -> Packet
parsePacket = parseLines packet

partOneI :: Packet -> Int
partOneI (Literal v _ _) = v
partOneI (Operator v _ ps) = sum (map partOneI ps) + v

evalOperator :: Int -> [Int] -> Int
evalOperator 0 ps = sum ps
evalOperator 1 ps = product ps
evalOperator 2 ps = minimum ps
evalOperator 3 ps = maximum ps
evalOperator 5 [a, b] = if a > b then 1 else 0
evalOperator 6 [a, b] = if a < b then 1 else 0
evalOperator 7 [a, b] = if a == b then 1 else 0

evaluate :: Packet -> Int
evaluate (Literal _ _ v) = v
evaluate (Operator _ t ps) = evalOperator t (map evaluate ps)

parseI :: String -> Challenge
parseI = parsePacket . concatMap decode

main :: IO ()
main = challenge 16 parseI partOneI evaluate