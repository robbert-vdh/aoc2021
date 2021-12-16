{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Char
import Data.List
import Numeric
import Text.Parsec
import Text.Printf

main :: IO ()
main = do
  Right !input <- parse pPacket "inputs/day-16.txt" . hexToBin <$> readFile "inputs/day-16.txt"

  putStrLn "Part 1:"
  print (sumVersions input)

  putStrLn "\nPart 2:"
  print (eval input)

-- * Part 1

-- | A straight up catamorphism for summing the versions because who has time to
-- abstract over this.
sumVersions :: Packet -> Int
sumVersions (Packet v b) =
  v + case b of
    Literal _             -> 0
    Operator _ subPackets -> sum $ map sumVersions subPackets


-- * Part 2

-- | Evaluate the expression in the packet
eval :: Packet -> Int
eval (Packet _ (Literal n))                   = n
eval (Packet _ (Operator Sum subPackets))     = sum      $ map eval subPackets
eval (Packet _ (Operator Product subPackets)) = product  $ map eval subPackets
eval (Packet _ (Operator Minimum subPackets)) = minimum  $ map eval subPackets
eval (Packet _ (Operator Maximum subPackets)) = maximum  $ map eval subPackets
eval (Packet _ (Operator GreaterThan [x, y])) = fromEnum $ eval x > eval y
eval (Packet _ (Operator LessThan [x, y]))    = fromEnum $ eval x < eval y
eval (Packet _ (Operator Equal [x, y]))       = fromEnum $ eval x == eval y
eval (Packet _ (Operator t subPackets)) =
  error $ "Invalid operator " <> show t <> " arguments: " <> show subPackets

-- * Parsing
--
-- Parsing will be the main thing today. Luckily, we're using Haskell.

type Parser = Parsec String ()

data Packet = Packet { version :: Int, body :: PacketBody }
  deriving (Show)

data PacketBody
  = Literal Int
  | Operator OperatorType [Packet]
  deriving (Show)

data OperatorType = Sum | Product | Minimum | Maximum | GreaterThan | LessThan | Equal
  deriving (Show)

-- | Parse a single packet (which may contain additional packets)
pPacket :: Parser Packet
pPacket = Packet <$> pBinUint 3 <*> pPacketBody

-- | Parse the body of a packet, as determined by the type header that follows
-- in the stream.
pPacketBody :: Parser PacketBody
pPacketBody = do
  packetType <- pBinUint 3
  case packetType of
    4 -> pPacketLiteral
    -- Anything that's doesn't have packet type 4 is an operator
    0 -> pPacketOperator Sum
    1 -> pPacketOperator Product
    2 -> pPacketOperator Minimum
    3 -> pPacketOperator Maximum
    5 -> pPacketOperator GreaterThan
    6 -> pPacketOperator LessThan
    7 -> pPacketOperator Equal
    n -> parserFail $ "Unknown operator type " <> show n

-- | Parses a packet for a literal number. This packet contains groups of 4 bits
-- that should all be concatenated and converted to a number.
pPacketLiteral :: Parser PacketBody
pPacketLiteral = do
  groups    <- many pLiteralGroup
  lastGroup <- pLastLiteralGroup
  return $! Literal (binUint $ concat groups ++ lastGroup)
  where
    pLiteralGroup     = char '1' *> count 4 pBinDigit
    pLastLiteralGroup = char '0' *> count 4 pBinDigit

-- | Parse an operator packet that can contain zero or more other packets. The
-- type has already been parsed, so it's passed here to this function.
pPacketOperator :: OperatorType -> Parser PacketBody
pPacketOperator operatorType = Operator operatorType <$> pSubPackets
  where
    pSubPackets = do
      lengthType <- pBinDigit
      case lengthType of
        -- The next 15 bits contain the total length in bits of the subpackets
        '0' -> do
          subPacketsLength <- pBinUint 15
          -- There may be a nicer way to parse exactly n characters here than
          -- doing nested parsing, but I couldn't find a suitable combinator
          subPacketsData <- count subPacketsLength anyChar
          case parse (many pPacket <* eof) "" subPacketsData of
            Right packets -> return packets
            Left _ ->
              parserFail $ "Could not parse '" <> subPacketsData <>
                           "' for " <> show subPacketsLength <> " packets"
        -- The next 11 bits contain the number of subpackets
        '1' -> do
          numSubPackets <- pBinUint 11
          count numSubPackets pPacket
        _ -> parserFail "This wasn't in our agreement!"

-- | Parse a zero or a one.
pBinDigit :: Parser Char
pBinDigit = oneOf "01"

-- | Parse exactly @n@ bits of the binary string to an unsigned integer.
pBinUint :: Int -> Parser Int
pBinUint n = binUint <$> count n pBinDigit

-- | Convert a string of binary digits to an unsigned integer.
binUint :: [Char] -> Int
binUint = foldl' (\acc c -> (acc * 2) + digitToInt c) 0

-- | Convert the hexadecimal encoded input string to a binary encoded input
-- string. All hexademical integers are left padded to four bits.
hexToBin :: String -> String
hexToBin = concatMap go
  where
    go hex =
      case readHex @Int [hex] of
        [(dec, _)] -> printf "%04b" dec
        _          -> error $ "Could not convert " <> [hex] <> " from hexadecmial"
