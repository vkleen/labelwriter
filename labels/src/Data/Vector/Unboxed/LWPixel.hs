{-# language PatternSynonyms #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Data.Vector.Unboxed.LWPixel ( LWPixel(On, Off)
                                   , U.Vector(LWPixelVec)
                                   , U.MVector(LWPixelMVec)
                                   ) where

import           Control.Monad.Primitive
import           Data.Bits.Utils
import qualified Data.Vector.Generic         as V
import qualified Data.Vector.Generic.Mutable as MV
import qualified Data.Vector.Unboxed         as U

newtype LWPixel = LWPixel Bool
  deriving (Bounded, Eq, Ord, Typeable)

pattern On :: LWPixel
pattern On  = LWPixel True
pattern Off :: LWPixel
pattern Off = LWPixel False

data instance U.Vector LWPixel = LWPixelVec {-# UNPACK #-} !Int
                                            {-# UNPACK #-} !Int
                                            !(U.Vector Word)

data instance U.MVector s LWPixel = LWPixelMVec {-# UNPACK #-} !Int
                                                {-# UNPACK #-} !Int
                                                !(U.MVector s Word)

replicateLWPixel :: LWPixel -> Word
replicateLWPixel (LWPixel False) = 0
replicateLWPixel (LWPixel True)  = complement 0


readLWPixel :: Int -> Word -> LWPixel
readLWPixel i w = LWPixel (testBit w i)

readWord :: PrimMonad m => U.MVector (PrimState m) LWPixel -> Int -> m Word
readWord (LWPixelMVec 0 n v) i
  | aligned i         = masked b <$> lo
  | j + 1 == nWords n = masked b <$> (extractWord k <$> lo <*> pure 0)
  | otherwise         = masked b <$> (extractWord k <$> lo <*> hi)
  where (j,k) = divModWordSize i
        b     = n - i
        lo    = MV.read v j
        hi    = MV.read v (j+1)

readWord (LWPixelMVec s n v) i = readWord (LWPixelMVec 0 (n+s) v) (i+s)

writeWord :: PrimMonad m => U.MVector (PrimState m) LWPixel -> Int -> Word -> m ()
writeWord (LWPixelMVec 0 n v) i x
  | aligned i = if b < wordSize
                then do y <- MV.read v j
                        MV.write v j (meld b x y)
                else MV.write v j x

  | j + 1 == nWords n = do
      lo <- MV.read v j
      let x'       = if b < wordSize
                     then meld b x (extractWord k lo 0)
                     else x
          (lo', _) = spliceWord k lo 0 x'
      MV.write v j lo'

  | otherwise = do
      lo <- MV.read v j
      hi <- MV.read v (j+1)
      let x'         = if b < wordSize
                       then meld b x (extractWord k lo hi)
                       else x
          (lo', hi') = spliceWord k lo hi x'
      MV.write v j     lo'
      MV.write v (j+1) hi'
  where (j,k) = divModWordSize i
        b = n - i

writeWord (LWPixelMVec s n v) i x = writeWord (LWPixelMVec 0 (n+s) v) (i+s) x

instance U.Unbox LWPixel

instance V.Vector U.Vector LWPixel where
  basicUnsafeFreeze (LWPixelMVec s n v) = LWPixelVec  s n <$> V.basicUnsafeFreeze v
  basicUnsafeThaw   (LWPixelVec  s n v) = LWPixelMVec s n <$> V.basicUnsafeThaw   v

  basicLength (LWPixelVec _ n _) = n

  basicUnsafeIndexM (LWPixelVec 0 _ v) i = readLWPixel k <$> V.basicUnsafeIndexM v j
    where (j, k) = divModWordSize i
  basicUnsafeIndexM (LWPixelVec s n v) i = V.basicUnsafeIndexM (LWPixelVec 0 (n+s) v) (i+s)

  basicUnsafeCopy dst src = do
    src' <- V.basicUnsafeThaw src
    MV.basicUnsafeCopy dst src'

  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice offset n (LWPixelVec s _ v) =
    LWPixelVec relStartBit n (V.basicUnsafeSlice startWord lenWords v)
    where absStartBit = s + offset
          relStartBit = modWordSize absStartBit
          absEndBit   = absStartBit + n
          endWord     = nWords absEndBit
          startWord   = divWordSize absStartBit
          lenWords    = endWord - startWord

instance MV.MVector U.MVector LWPixel where
  basicInitialize (LWPixelMVec _ _ v) = MV.basicInitialize v
  basicUnsafeNew n = LWPixelMVec 0 n <$> MV.basicUnsafeNew (nWords n)
  basicUnsafeReplicate n x = LWPixelMVec 0 n <$> MV.basicUnsafeReplicate (nWords n) (replicateLWPixel x)

  basicOverlaps (LWPixelMVec _ _ v1) (LWPixelMVec _ _ v2) = MV.basicOverlaps v1 v2

  basicLength (LWPixelMVec _ n _) = n

  basicUnsafeRead (LWPixelMVec 0 _ v) i = readLWPixel k <$> MV.basicUnsafeRead v j
    where (j,k) = divModWordSize i
  basicUnsafeRead (LWPixelMVec s n v) i = MV.basicUnsafeRead (LWPixelMVec 0 (n+s) v) (i+s)

  basicUnsafeWrite (LWPixelMVec 0 _ v) i (LWPixel x) = do
    let (j,k) = divModWordSize i
    w <- MV.basicUnsafeRead v j
    MV.basicUnsafeWrite v j $ (if x then setBit else clearBit) w k
  basicUnsafeWrite (LWPixelMVec s n v) i x = MV.basicUnsafeWrite (LWPixelMVec 0 (n+s) v) (i+s) x

  basicSet (LWPixelMVec _ _ v) x = MV.basicSet v (replicateLWPixel x)

  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy dst@(LWPixelMVec _ len _) src = do_copy 0
    where n = alignUp len
          do_copy i | i < n = do
                        x <- readWord src i
                        writeWord dst i x
                        do_copy (i + wordSize)
                    | otherwise = return ()

  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice offset n (LWPixelMVec s _ v) =
    LWPixelMVec relStartBit n (MV.basicUnsafeSlice startWord lenWords v)
    where absStartBit = s + offset
          relStartBit = modWordSize absStartBit
          absEndBit   = absStartBit + n
          endWord     = nWords absEndBit
          startWord   = divWordSize absStartBit
          lenWords    = endWord - startWord
