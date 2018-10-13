{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Data.Vector.Unboxed.Pixel ( Pixel
                                 , U.Vector(PixelVec)
                                 , U.MVector(PixelMVec)
                                 ) where

import           Control.Monad.Primitive
import           Data.Bits.Utils
import qualified Data.Vector.Generic         as V
import qualified Data.Vector.Generic.Mutable as MV
import qualified Data.Vector.Unboxed         as U

newtype Pixel = Pixel Bool
  deriving (Bounded, Eq, Ord, Typeable)

data instance U.Vector Pixel = PixelVec {-# UNPACK #-} !Int
                                        {-# UNPACK #-} !Int
                                        !(U.Vector Word)

data instance U.MVector s Pixel = PixelMVec {-# UNPACK #-} !Int
                                            {-# UNPACK #-} !Int
                                            !(U.MVector s Word)

replicatePixel :: Pixel -> Word
replicatePixel (Pixel False) = 0
replicatePixel (Pixel True)  = complement 0


readPixel :: Int -> Word -> Pixel
readPixel i w = Pixel (testBit w i)

readWord :: PrimMonad m => U.MVector (PrimState m) Pixel -> Int -> m Word
readWord (PixelMVec 0 n v) i
  | aligned i         = masked b <$> lo
  | j + 1 == nWords n = masked b <$> (extractWord k <$> lo <*> pure 0)
  | otherwise         = masked b <$> (extractWord k <$> lo <*> hi)
  where (j,k) = divModWordSize i
        b     = n - i
        lo    = MV.read v j
        hi    = MV.read v (j+1)

readWord (PixelMVec s n v) i = readWord (PixelMVec 0 (n+s) v) (i+s)

writeWord :: PrimMonad m => U.MVector (PrimState m) Pixel -> Int -> Word -> m ()
writeWord (PixelMVec 0 n v) i x
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

writeWord (PixelMVec s n v) i x = writeWord (PixelMVec 0 (n+s) v) (i+s) x

instance U.Unbox Pixel

instance V.Vector U.Vector Pixel where
  basicUnsafeFreeze (PixelMVec s n v) = PixelVec  s n <$> V.basicUnsafeFreeze v
  basicUnsafeThaw   (PixelVec  s n v) = PixelMVec s n <$> V.basicUnsafeThaw   v

  basicLength (PixelVec _ n _) = n

  basicUnsafeIndexM (PixelVec 0 _ v) i = readPixel k <$> V.basicUnsafeIndexM v j
    where (j, k) = divModWordSize i
  basicUnsafeIndexM (PixelVec s n v) i = V.basicUnsafeIndexM (PixelVec 0 (n+s) v) (i+s)

  basicUnsafeCopy dst src = do
    src' <- V.basicUnsafeThaw src
    MV.basicUnsafeCopy dst src'

  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice offset n (PixelVec s _ v) =
    PixelVec relStartBit n (V.basicUnsafeSlice startWord lenWords v)
    where absStartBit = s + offset
          relStartBit = modWordSize absStartBit
          absEndBit   = absStartBit + n
          endWord     = nWords absEndBit
          startWord   = divWordSize absStartBit
          lenWords    = endWord - startWord

instance MV.MVector U.MVector Pixel where
  basicInitialize (PixelMVec _ _ v) = MV.basicInitialize v
  basicUnsafeNew n = PixelMVec 0 n <$> MV.basicUnsafeNew (nWords n)
  basicUnsafeReplicate n x = PixelMVec 0 n <$> MV.basicUnsafeReplicate (nWords n) (replicatePixel x)

  basicOverlaps (PixelMVec _ _ v1) (PixelMVec _ _ v2) = MV.basicOverlaps v1 v2

  basicLength (PixelMVec _ n _) = n

  basicUnsafeRead (PixelMVec 0 _ v) i = readPixel k <$> MV.basicUnsafeRead v j
    where (j,k) = divModWordSize i
  basicUnsafeRead (PixelMVec s n v) i = MV.basicUnsafeRead (PixelMVec 0 (n+s) v) (i+s)

  basicUnsafeWrite (PixelMVec 0 _ v) i (Pixel x) = do
    let (j,k) = divModWordSize i
    w <- MV.basicUnsafeRead v j
    MV.basicUnsafeWrite v j $ (if x then setBit else clearBit) w k
  basicUnsafeWrite (PixelMVec s n v) i x = MV.basicUnsafeWrite (PixelMVec 0 (n+s) v) (i+s) x

  basicSet (PixelMVec _ _ v) x = MV.basicSet v (replicatePixel x)

  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy dst@(PixelMVec _ len _) src = do_copy 0
    where n = alignUp len
          do_copy i | i < n = do
                        x <- readWord src i
                        writeWord dst i x
                        do_copy (i + wordSize)
                    | otherwise = return ()

  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice offset n (PixelMVec s _ v) =
    PixelMVec relStartBit n (MV.basicUnsafeSlice startWord lenWords v)
    where absStartBit = s + offset
          relStartBit = modWordSize absStartBit
          absEndBit   = absStartBit + n
          endWord     = nWords absEndBit
          startWord   = divWordSize absStartBit
          lenWords    = endWord - startWord
