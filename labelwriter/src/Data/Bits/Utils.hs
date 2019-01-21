module Data.Bits.Utils where

import Prelude hiding (mask)

import Data.List

wordSize :: Int
wordSize = finiteBitSize (0 :: Word)

lg2 :: Int -> Int
lg2 n = i
    where Just i = findIndex (>= toInteger n) (iterate (`shiftL` 1) 1)

lgWordSize, wordSizeMask, wordSizeMaskC :: Int
lgWordSize = lg2 wordSize
wordSizeMask = wordSize - 1
wordSizeMaskC = complement wordSizeMask

divWordSize, modWordSize, mulWordSize :: Int -> Int
divWordSize x = shiftR x lgWordSize
modWordSize x = x .&. (wordSize - 1)

divModWordSize :: Int -> (Int, Int)
divModWordSize x = (divWordSize x, modWordSize x)

mulWordSize x = shiftL x lgWordSize

-- number of words needed to store n bits
nWords :: Int -> Int
nWords n = divWordSize (n + wordSize - 1)

-- number of bits storable in n words
nBits :: Int -> Int
nBits = mulWordSize

aligned, notAligned :: Int -> Bool
aligned    x = (x .&. wordSizeMask == 0)
notAligned x = x /= alignDown x

alignUp, alignDown :: Int -> Int
-- round a number of bits up to the nearest multiple of word size
alignUp x
    | x == x'   = x'
    | otherwise = x' + wordSize
    where x' = alignDown x
-- round a number of bits down to the nearest multiple of word size
alignDown x = x .&. wordSizeMaskC

-- create a mask consisting of the lower b bits
mask :: Int -> Word
mask b = m
    where m | b >= finiteBitSize m = complement 0
            | b < 0                = 0
            | otherwise            = bit b - 1

masked :: Int -> Word -> Word
masked b x = x .&. mask b

isMasked :: Int -> Word -> Bool
isMasked b x = (masked b x == x)

-- given a bit offset 'k' and 2 words, extract a word by taking the 'k' highest bits of the first word and the 'wordSize - k' lowest bits of the second word.
{-# INLINE extractWord #-}
extractWord :: Int -> Word -> Word -> Word
extractWord k lo hi = (lo `shiftR` k) .|. (hi `shiftL` (wordSize - k))

-- given a bit offset 'k', 2 words 'lo' and 'hi' and a word 'x', overlay 'x' onto 'lo' and 'hi' at the position such that (k `elem` [0..wordSize] ==> uncurry (extractWord k) (spliceWord k lo hi x) == x) and (k `elem` [0..wordSize] ==> spliceWord k lo hi (extractWord k lo hi) == (lo,hi))
{-# INLINE spliceWord #-}
spliceWord :: Int -> Word -> Word -> Word -> (Word, Word)
spliceWord k lo hi x = ( meld k lo (x `shiftL` k)
                       , meld k (x `shiftR` (wordSize - k)) hi
                       )


-- meld 2 words by taking the low 'b' bits from 'lo' and the rest from 'hi'
meld :: Int -> Word -> Word -> Word
meld b lo hi = (lo .&. m) .|. (hi .&. complement m)
    where m = mask b
