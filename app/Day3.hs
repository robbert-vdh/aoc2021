{-# LANGUAGE BangPatterns #-}

module Main where

import Data.Bits
import Data.Foldable (foldl')
import Data.Maybe (fromJust)
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

main :: IO ()
main = do
  !input <- parse <$> readFile "inputs/day-3.txt"
  let numInputs      = length input -- Linked lists go brrrrrrr
      !bitCounts     = countBits input emptyBitCounts
      !bitMajorities = calculateBitMajories numInputs bitCounts

  putStrLn "Part 1:"
  print . powerConsumption . buildGamma $ bitMajorities

  putStrLn "\nPart 1:"
  let !oxygenGenerator = buildGamma . V.fromList . map (== '1') . fromJust $ findMatch Keep input
      !co2Scrubber     = buildGamma . V.fromList . map (== '1') . fromJust $ findMatch Reject input
  print $ lifeSupport oxygenGenerator co2Scrubber

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

-- | Contains how many entries have the bit with the corresponding.
type BitCounts = Vector Int
-- | Contains whether the majority of the entries have the corresponding bit
-- set.
type BitMajorities = Vector Bool

emptyBitCounts :: BitCounts
emptyBitCounts = V.replicate wordSize 0

-- | Increase the count for the given bit index by one. @bitIdx@ should be in
-- the range @[0, wordSize - 1]@.
incBitCount :: Int -> BitCounts -> BitCounts
incBitCount bitIdx = V.modify $ \mv -> MV.unsafeModify mv (+ 1) bitIdx

type Gamma = Int
type OxygenGenerator = Int
type Co2Scrubber = Int

powerConsumption :: Gamma -> Int
powerConsumption gamma = gamma * epsilon
  where
    epsilon = complement gamma .&. wordMask

lifeSupport :: OxygenGenerator -> Co2Scrubber -> Int
lifeSupport = (*)

-- | Count the total number of one bits for every individual bit in @[0,
-- wordLength - 1]@ for all bit strings in the list.
countBits :: [BitString] -> BitCounts -> BitCounts
countBits (x : xs) counts = countBits xs $ foldl' updateCounts counts (zip x wordBits)
  where
    updateCounts :: BitCounts -> (Char, Int) -> BitCounts
    updateCounts counts' ('1', bitIdx) = incBitCount bitIdx counts'
    updateCounts counts' _             = counts'
countBits [] counts = counts

calculateBitMajories :: Int -> BitCounts -> BitMajorities
calculateBitMajories numInputs = V.map (>= threshold)
  where
    threshold :: Int
    threshold = ceiling $ (fromIntegral numInputs :: Float) / 2

-- | Build the gamma rate, which has its bits set to 1 if the majority of the
-- same bits in the bit counts is also set to 1. This is of course dependent on
-- the total number of inputs.
buildGamma :: BitMajorities -> Gamma
buildGamma majorities = foldl' maybeSetBit 0 wordBits
  where
    maybeSetBit :: Int -> Int -> Int
    maybeSetBit acc bitIdx
      | majorities V.! bitIdx = setBit acc bitIdx
      | otherwise             = acc

-- | Whether to keep or to reject entries where the bit positions match the most
-- common values in the bit counts. Keep corresponds to the oxygen generator
-- rating, while reject corresponds to the CO2 scrubber rating.
data FindMode = Keep | Reject deriving (Eq)

-- | Find the first element that matches the matching rules from
-- https://adventofcode.com/2021/day/3#part2.
--
-- NOTE: The 'first bit' in the bit exercise is not the 'first bit', but the
--       leftmost bit, or the most significant bit. That means that we need to
--       count backwards when going through the bit indices.
findMatch :: FindMode -> [BitString] -> Maybe BitString
findMatch mode input = findFirstMatch (wordSize - 1) 0 (calculateMajorities input) input
  where
    -- | This is super convoluted because we need to modify lists in place and
    -- also keep track of the current set of majorities (which only change when
    -- going to the next bit). And yes, this is super slow and ugly using lists,
    -- I know.
    findFirstMatch :: Int -> Int -> BitMajorities -> [BitString] -> Maybe BitString
    findFirstMatch (-1)   _       _          _   = Nothing
    findFirstMatch _      _       _          []  = Nothing
    findFirstMatch _      _       _          [x] = Just x
    findFirstMatch bitIdx elemIdx majorities xs
        -- Recompute the majorities when going to the next bit
      | elemIdx == length xs
      = findFirstMatch (bitIdx - 1) 0             (calculateMajorities xs) xs
      | (mode == Keep   && currentBit == majorityBit) ||
        (mode == Reject && currentBit /= majorityBit)
      = findFirstMatch bitIdx       (elemIdx + 1) majorities               xs
      | otherwise
      = findFirstMatch bitIdx       elemIdx       majorities               $ take elemIdx xs ++ drop (elemIdx + 1) xs
      where
        currentBit  = xs !! elemIdx !! bitIdx == '1'
        majorityBit = majorities V.! bitIdx

    calculateMajorities xs = calculateBitMajories (length xs) $ countBits xs emptyBitCounts

-- | Parse the input strings to lists of characters that should contain only
-- either @'0'@s or @'1'@s. We could also parse these to integers and test bits
-- that way, but we already have this format so this is easier to work with. The
-- lists need to be reversed so the least significant bit comes first.
parse :: String -> [BitString]
parse = map reverse . lines
