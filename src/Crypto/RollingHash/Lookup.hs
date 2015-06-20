module Crypto.RollingHash.Lookup
  ( Word32Map
  , fromListW32, lookupW32
  ) where

-- [Note: Order of constructors]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- The order of constructors of IntMap matters when considering performance.
-- Currently in GHC 7.0, when type has 3 constructors, they are matched from
-- the first to the last -- the best performance is achieved when the
-- constructors are ordered by frequency.

import Control.Monad
import Data.Bits
import Data.List
import Data.Word
import Foreign
import System.IO.Unsafe

type Array a = ForeignPtr a

data Word32Map a = Word32Map
  { _word16array :: Array Int      -- ^ positions in lookuplist
  , _word16len   :: Array Word16   -- ^ number of elements at current word16
  , _word32array :: Array Word32
  , _elemarray   :: Array a
  }

-- | Take the first 16 bits of a 32 bit unsigned integer
takeFst16 :: Word32 -> Word16
takeFst16 w32 = fromIntegral $ shiftR w32 16

-- for type safety:

w16toInt :: Word16 -> Int
w16toInt = fromIntegral
{-# INLINE w16toInt #-}

{-# INLINABLE fromListW32 #-}
fromListW32 :: (Storable a, Ord a) => [(Word32,a)] -> Word32Map a
fromListW32 l = unsafePerformIO $ do

  let (w32_l,elem_l) = unzip $ sort l

  -- poke lists into arrays
  fw32_a  <- toForeignPtr $ newArray w32_l
  felem_a <- toForeignPtr $ newArray elem_l

  -- create Word16 array
  fw16_a  <- toForeignPtr $ newArray $ replicate (2^(16 :: Int)) 0
  fw16l_a <- toForeignPtr $ newArray $ replicate (2^(16 :: Int)) 0

  forM_ (zip w32_l [0..]) $ \(w32,p) ->
    withForeignPtr fw16_a $ \ w16_a -> do
    withForeignPtr fw16l_a $ \w16l_a -> do
      let w16  = takeFst16 w32
          ptr  = advancePtr w16_a  (w16toInt w16)
          ptr' = advancePtr w16l_a (w16toInt w16)
      -- store position
      p' <- peek ptr
      when (p' == 0) $
        poke ptr (p+1)
      -- increase counter
      l' <- peek ptr'
      poke ptr' (l'+1)

  return $ Word32Map fw16_a fw16l_a fw32_a felem_a

 where

  toForeignPtr = (>>= newForeignPtr finalizerFree)

{-# INLINABLE lookupW32 #-}
lookupW32 :: Storable a => Word32 -> Word32Map a -> Maybe a
lookupW32 w32 (Word32Map fw16_a fw16l_a fw32_a felem_a) =
  unsafePerformIO $
  withForeignPtr fw16_a $ \w16_a ->
  withForeignPtr fw16l_a $ \w16l_a -> do

    -- lookup first occurance (if any) of the Word16 part in the array
    p <- peek (advancePtr w16_a  (w16toInt w16))
    -- number of entries at current position
    l <- peek (advancePtr w16l_a (w16toInt w16))

    if l > 0 then go l (p-1) else return Nothing

 where
  go 0 _ = return Nothing
  go l p = do
    withForeignPtr fw32_a $ \w32_a -> do
    withForeignPtr felem_a $ \elem_a -> do
      -- compare the current value with the wanted one
      w32' <- peek (advancePtr w32_a p)
      if w32 == w32' then do
         -- success
         el <- peek (advancePtr elem_a p)
         return $ Just el
       else
         -- continue looking at next position
         go (l-1) (p+1)

  w16 = takeFst16 w32
