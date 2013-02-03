{-# LANGUAGE BangPatterns #-}

-- | Pure Haskell implementation of the Rolling Hash algorithm
module Crypto.RollingHash.Pure
  ( -- * Types
    R, mkR, Rolling (emptyR, isEmptyR)
    -- * Rolling
  , roll, rollN, rollBlock
    -- * Hashing
  , hashR32, hashR16
  ) where

import Data.Word

import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as BL

import Crypto.RollingHash.Internal

-- | Roll to the next byte
roll :: Rolling a => R a -> R a
roll !r = {-# SCC "roll" #-}
  case splitR (dat_0 r) of { (h_0,t_0) ->
  case splitR (dat_l r) of { (h_l,t_l) ->
   r { dat_0 = t_0
     , dat_l = t_l
       -- a_1 = a_0 - x_0 + x_{l+1}
     , hsh_a = {-# SCC "hsh_a" #-} hsh_a r - fromIntegral h_0 + fromIntegral h_l
       -- b_1 = b_0 - (l+1) * x_0 + a_1
       --     = b_0 - (l+1) * x_0 + a_0 - x_0 + x_{l+1}
       --     = b_0 + a_0 - l * x_0 + x_{l+1}
     , hsh_b = {-# SCC "hsh_b" #-} hsh_b r + hsh_a r
             - fromIntegral (var_l r * h_0) + fromIntegral h_l
     }
  }}

{-# SPECIALIZE roll :: R [Word8]       -> R [Word8]       #-}
{-# SPECIALIZE roll :: R BS.ByteString -> R BS.ByteString #-}
{-# SPECIALIZE roll :: R BL.ByteString -> R BL.ByteString #-}

-- | Roll @N@ bytes
rollN :: Rolling a => Int -> R a -> R a
rollN !i !r = {-# SCC "rollN" #-} case i of
  0 -> r
  _ -> rollN (i-1) (roll r)

-- | Roll to the next block with the specified block length
rollBlock :: Rolling a => R a -> R a
rollBlock r = {-# SCC "rollBlock" #-} r
  { dat_0 = a_0
  , dat_l = a_l
  , hsh_a = r1 (fromIntegral l) a_0
  , hsh_b = r2 (fromIntegral l) a_0
  } 
 where
  l = var_l r
  a_0 = dropR (fromIntegral l) (dat_0 r)
  a_l = dropR (fromIntegral l) (dat_l r)

-- | Hash @R a@ by building a 'Word32' from both 'Word16' parameters. This is
-- equivalent to @a * 2^16 + b@.
hashR32 :: Rolling a => R a -> Word32
hashR32 r = buildWord32' (hsh_a r) (hsh_b r)

-- | Hash @R a@ by simply adding both 'Word16' values for @a@ and @b@.
hashR16 :: Rolling a => R a -> Word16
hashR16 r = hsh_a r + hsh_b r
