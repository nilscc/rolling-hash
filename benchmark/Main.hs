module Main where

import           Criterion
import           Criterion.Main
import           Crypto.RollingHash
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Word
import           System.Random

main :: IO ()
main = do

  bs <- buildBS 100000
  let r = mkR 100 bs

  defaultMain [ bench "mkR 100" $ whnf (mkR 100) bs

              , bcompare [ bench "roll"      $ whnf benchRoll   r
                         , bench "roll32"    $ whnf benchRoll32 r
                         , bench "roll16"    $ whnf benchRoll16 r
                         ]

              , bcompare [ bench "rollBlock"   $ whnf benchRollBlock   r
                         , bench "rollBlock32" $ whnf benchRollBlock32 r
                         , bench "rollBlock16" $ whnf benchRollBlock16 r
                         ]

              , bcompare [ bench "rollBlockWithN"   $ whnf benchRollBlockWithN   r
                         , bench "rollBlockWithN32" $ whnf benchRollBlockWithN32 r
                         , bench "rollBlockWithN16" $ whnf benchRollBlockWithN16 r
                         ]

              , bcompare [ bench "rollN"   $ whnf (benchRollN   100) r
                         , bench "rollN32" $ whnf (benchRollN32 100) r
                         , bench "rollN16" $ whnf (benchRollN16 100) r
                         ]

              , bcompare [ bench "rollBlock"        $ whnf benchRollBlock      r
                         , bench "rollBlockWithN"   $ whnf benchRollBlockWithN r
                         , bench "rollN"            $ whnf (benchRollN 100)    r
                         ]
              ]

buildBS :: Int -> IO ByteString
buildBS len = do
  let loop i w8s = if i >= len then return w8s else do
        w8 <- randomIO
        loop (i+1) (w8:w8s)
  w8s <- loop 0 []
  return $ BS.pack w8s

--------------------------------------------------------------------------------
-- roll

benchRoll :: R ByteString -> Int
benchRoll r =
  let loop r' = if isEmptyR r'
                    then []
                    else hashRInt r' : loop (roll r')
   in last $ loop r

benchRoll32 :: R ByteString -> Word32
benchRoll32 r =
  let loop r' = if isEmptyR r'
                    then []
                    else hashR32 r' : loop (roll r')
   in last $ loop r

benchRoll16 :: R ByteString -> Word16
benchRoll16 r =
  let loop r' = if isEmptyR r'
                    then []
                    else hashR16 r' : loop (roll r')
   in last $ loop r

--------------------------------------------------------------------------------
-- rollN

benchRollN :: Int -> R ByteString -> Int
benchRollN n r =
  let loop r' = if isEmptyR r'
                    then []
                    else hashRInt r' : loop (rollN n r')
   in last $ loop r

benchRollN16 :: Int -> R ByteString -> Word16
benchRollN16 n r =
  let loop r' = if isEmptyR r'
                    then []
                    else hashR16 r' : loop (rollN n r')
   in last $ loop r

benchRollN32 :: Int -> R ByteString -> Word32
benchRollN32 n r =
  let loop r' = if isEmptyR r'
                    then []
                    else hashR32 r' : loop (rollN n r')
   in last $ loop r

--------------------------------------------------------------------------------
-- rollBlockWithN

benchRollBlockWithN :: R ByteString -> Int
benchRollBlockWithN r =
  let loop r' = if isEmptyR r'
                    then []
                    else hashRInt r' : loop (rollBlockWithN r')
   in last $ loop r

benchRollBlockWithN16 :: R ByteString -> Word16
benchRollBlockWithN16 r =
  let loop r' = if isEmptyR r'
                    then []
                    else hashR16 r' : loop (rollBlockWithN r')
   in last $ loop r

benchRollBlockWithN32 :: R ByteString -> Word32
benchRollBlockWithN32 r =
  let loop r' = if isEmptyR r'
                    then []
                    else hashR32 r' : loop (rollBlockWithN r')
   in last $ loop r

--------------------------------------------------------------------------------
-- rollBlock

benchRollBlock :: R ByteString -> Int
benchRollBlock r =
  let loop r' = if isEmptyR r'
                    then []
                    else hashRInt r' : loop (rollBlock r')
   in last $ loop r

benchRollBlock16 :: R ByteString -> Word16
benchRollBlock16 r =
  let loop r' = if isEmptyR r'
                    then []
                    else hashR16 r' : loop (rollBlock r')
   in last $ loop r

benchRollBlock32 :: R ByteString -> Word32
benchRollBlock32 r =
  let loop r' = if isEmptyR r'
                    then []
                    else hashR32 r' : loop (rollBlock r')
   in last $ loop r
