{-# LANGUAGE BangPatterns #-}

-- | Pure Haskell implementation of the Rolling Hash algorithm
module Crypto.RollingHash.Pure
  ( -- * Types
    R, mkR, Rolling (emptyR, isEmptyR)
    -- * Rolling
  , roll, rollN, rollBlock, rollBlockWithN
    -- * Hashing
  , hashRInt, hashR32, hashR16
  ) where

import Data.Word

import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as BL

import Crypto.RollingHash.Internal

-- | Roll to the next byte
roll :: Rolling a => R a -> R a
roll !r =
  case splitR (dat_0 r) of { (h_0,t_0) ->
  case splitR (dat_l r) of { (h_l,t_l) ->
   r { dat_0 = t_0
     , dat_l = t_l

       -- a_1 = a_0 - x_0 + x_{l+1}
     , hsh_a = hsh_a r - fromIntegral h_0 + fromIntegral h_l

       -- b_1 = b_0 - (l+1) * x_0 + a_1
       --     = b_0 - (l+1) * x_0 + a_0 - x_0 + x_{l+1}
       --     = b_0 + a_0 - l * x_0 + x_{l+1}

     , hsh_b = let a,b,h0,hl,i :: Int
                   a  = fromIntegral (hsh_a r)
                   b  = fromIntegral (hsh_b r)
                   h0 = fromIntegral h_0
                   hl = fromIntegral h_l
                   i  = a + b - var_l r * h0 + hl
                   {-# INLINE a #-}
                   {-# INLINE b #-}
                   {-# INLINE h0 #-}
                   {-# INLINE hl #-}
                   {-# INLINE i #-}
                in fromIntegral i
     }
  }}

{-# SPECIALIZE roll :: R [Word8]       -> R [Word8]       #-}
{-# SPECIALIZE roll :: R BS.ByteString -> R BS.ByteString #-}
{-# SPECIALIZE roll :: R BL.ByteString -> R BL.ByteString #-}

-- | Roll @N@ bytes
rollN :: Rolling a => Int -> R a -> R a
rollN !i !r = case i of
  0 -> r
  _ -> rollN (i-1) (roll r)

{-# SPECIALIZE rollN :: Int -> R [Word8]       -> R [Word8]       #-}
{-# SPECIALIZE rollN :: Int -> R BS.ByteString -> R BS.ByteString #-}
{-# SPECIALIZE rollN :: Int -> R BL.ByteString -> R BL.ByteString #-}

-- | Roll to the next block with the specified block length
rollBlock :: Rolling a => R a -> R a
rollBlock r = r
  { dat_0 = a_0
  , dat_l = a_l
  , hsh_a = r1 (fromIntegral l) a_0
  , hsh_b = r2 (fromIntegral l) a_0
  } 
 where
  l = var_l r
  a_0 = dropR (fromIntegral l) (dat_0 r)
  a_l = dropR (fromIntegral l) (dat_l r)

-- | Roll to the next block with the specified block length
rollBlockWithN :: Rolling a => R a -> R a
rollBlockWithN r = rollN (var_l r) r

{-# SPECIALIZE rollBlock :: R [Word8]       -> R [Word8]       #-}
{-# SPECIALIZE rollBlock :: R BS.ByteString -> R BS.ByteString #-}
{-# SPECIALIZE rollBlock :: R BL.ByteString -> R BL.ByteString #-}

hashRInt :: Rolling a => R a -> Int
hashRInt r = fromIntegral $ hashR32 r

-- | Hash @R a@ by building a 'Word32' from both 'Word16' parameters. This is
-- equivalent to @a * 2^16 + b@.
hashR32 :: Rolling a => R a -> Word32
hashR32 r = buildWord32' (hsh_a r) (hsh_b r)

-- | Hash @R a@ by simply adding both 'Word16' values for @a@ and @b@.
hashR16 :: Rolling a => R a -> Word16
hashR16 r = hsh_a r + hsh_b r
