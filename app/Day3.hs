{-# LANGUAGE BangPatterns    #-}

module Main where

import Data.Bits
import Data.Foldable (foldl')
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

main :: IO ()
main = do
  !input <- parse <$> readFile "inputs/day-3.txt"
  let numInputs = length input -- Linked lists go brrrrrrr

  putStrLn "Part 1:"
  print . powerConsumption . buildGamma numInputs $! countBits input emptyBitCounts

-- | Represents one line of the input. These should be reversed so they match up
-- so the least significant bit comes first.
type BitString = [Char]

-- | The number of bits in each entry.
--
-- NOTE: This is the number of character in each input line, and it's hardcoded
--       for the solution because why not.
wordSize :: Int
wordSize = 12

-- | A bitmask for the correct size entry, since we use 12 bits.
wordMask :: Int
wordMask = (1 `shiftL` wordSize) - 1

wordBits :: [Int]
wordBits = [0..wordSize - 1]

type BitCounts = Vector Int

emptyBitCounts :: BitCounts
emptyBitCounts = V.replicate wordSize 0

-- | Increase the count for the given bit index by one. @bitIdx@ should be in
-- the range @[0, wordSize - 1]@.
incBitCount :: Int -> BitCounts -> BitCounts
incBitCount bitIdx = V.modify $ \mv -> MV.unsafeModify mv (+ 1) bitIdx

type Gamma = Int

powerConsumption :: Gamma -> Int
powerConsumption gamma = gamma * epsilon
  where
    epsilon = complement gamma .&. wordMask

-- | Count the total number of one bits for every individual bit in @[0,
-- wordLength - 1]@ for all bit strings in the list.
countBits :: [BitString] -> BitCounts -> BitCounts
countBits (x : xs) counts = countBits xs $ foldl' updateCounts counts (zip x wordBits)
  where
    updateCounts :: BitCounts -> (Char, Int) -> BitCounts
    updateCounts counts' ('1', bitIdx) = incBitCount bitIdx counts'
    updateCounts counts' _             = counts'
countBits [] counts = counts

-- | Build the gamma rate, which has its bits set to 1 if the majority of the
-- same bits in the bit counts is also set to 1. This is of course dependent on
-- the total number of inputs.
buildGamma :: Int -> BitCounts -> Gamma
buildGamma numInputs counts = foldl' maybeSetBit 0 wordBits
  where
    maybeSetBit :: Int -> Int -> Int
    maybeSetBit acc bitIdx
      | counts V.! bitIdx >= threshold = setBit acc bitIdx
      | otherwise          = acc

    threshold = numInputs `div` 2

-- | Parse the input strings to lists of characters that should contain only
-- either @'0'@s or @'1'@s. We could also parse these to integers and test bits
-- that way, but we already have this format so this is easier to work with. The
-- lists need to be reversed so the least significant bit comes first.
parse :: String -> [BitString]
parse = map reverse . lines
