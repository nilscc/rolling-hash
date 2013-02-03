{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}

module Crypto.RollingHash where

import Data.ByteString (ByteString)
import Data.Word
import Foreign hiding (unsafePerformIO)
import Foreign.C
import System.IO.Unsafe

import qualified Data.ByteString          as BS
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Unsafe   as BU

type Blocksize = Int

newtype R a = R { r_ptr :: ForeignPtr (R a) }

{-# INLINE withByteStringPtr #-}
withByteStringPtr :: ByteString -> (Ptr Word8 -> IO a) -> IO a
withByteStringPtr b f =
  case BI.toForeignPtr b of { (fptr, off, _) ->
    withForeignPtr fptr $ \ptr -> f (ptr `plusPtr` off)
  }


foreign import ccall unsafe "rolling.h rolling_init"
  c_rolling_init
    :: Ptr Word8
    -> Word32
    -> Word32
    -> IO (Ptr (R a))

foreign import ccall unsafe "rolling.h &rolling_finalize"
  c_rolling_finalize
    :: FunPtr (Ptr (R a) -> IO ())

initR :: ByteString -> Blocksize -> R ByteString
initR bs l = unsafePerformIO $ do
  ptr <- BU.unsafeUseAsCStringLen bs $ \(cs,len) ->
    c_rolling_init (castPtr cs) (fromIntegral len) (fromIntegral l)
  R `fmap` newForeignPtr c_rolling_finalize ptr

foreign import ccall unsafe "rolling.h rolling_hash32"
  c_rolling_hash32
    :: Ptr (R a)
    -> IO Word32

hashR32 :: R ByteString -> Word32
hashR32 (R fptr) = unsafePerformIO $
  withForeignPtr fptr $ \ptr ->
    c_rolling_hash32 ptr
