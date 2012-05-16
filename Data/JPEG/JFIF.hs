module Data.JPEG.JFIF where

import Control.DeepSeq
import Data.Ratio
import Data.Word
import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Generic.Base as GB

import Data.JPEG.Instances
import Data.JPEG.JPEGState
import Data.JPEG.Util

isJFIF :: JPEGState -> Bool
isJFIF _ = True
{-
isJFIF s = not $ null jfifs
  where app0s = [snd v | v <- applicationData s, fst v == 0]
        jfifs = filter (\ bs -> BS.take 5 bs == BS.pack [0x4A, 0x46, 0x49, 0x46, 0x00]) $ app0s
-}

resample :: (Integral a, U.Unbox a) => Rational -> Rational -> V.Vector (U.Vector a) -> V.Vector (U.Vector a)
resample xfrac yfrac values = V.generate (lengthfunc yfrac $ V.length values) f
  where lengthfunc frac x = truncate $ (1 / frac) * ((fromIntegral x) % 1)
        f y = g $ values V.! (truncate $ yfrac * ((fromIntegral y) % 1))
        g xs = U.generate (lengthfunc xfrac $ U.length xs) h
          where h x = xs U.! (truncate $ xfrac * ((fromIntegral x) % 1))

ycbcr2rgb :: V.Vector (V.Vector (U.Vector Int)) -> V.Vector (V.Vector (U.Vector Int))
ycbcr2rgb i = V.fromList [r, g, b]
  where y = i V.! 0
        cb = i V.! 1
        cr = i V.! 2
        r = V.zipWith3 (U.zipWith3 $ t fr) y cb cr
        g = V.zipWith3 (U.zipWith3 $ t fg) y cb cr
        b = V.zipWith3 (U.zipWith3 $ t fb) y cb cr
        t f a b c = floor $ clamp 0 255 $ f (fromIntegral a) (fromIntegral b) (fromIntegral c)
        fr y' cb' cr' = y'                         + 1.402   * (cr' - 128)
        fg y' cb' cr' = y' - 0.34414 * (cb' - 128) - 0.71414 * (cr' - 128)
        fb y' cb' cr' = y' + 1.772   * (cb' - 128)

convertJFIFImage :: JPEGState -> M.Map Word8 (V.Vector (U.Vector Int)) -> V.Vector (V.Vector (U.Vector Int))
convertJFIFImage s m = rgb `deepseq` rgb
  where order = L.sort $ M.keys m
        resampled = M.mapWithKey (\ k v -> resample (xratio k) (yratio k) v) m
        xratio c = (fromIntegral $ h $ (frameComponents $ frameHeader s) M.! c) % (fromIntegral maxx)
        yratio c = (fromIntegral $ v $ (frameComponents $ frameHeader s) M.! c) % (fromIntegral maxy)
        maxx = foldl1 max $ map h $ M.elems $ frameComponents $ frameHeader s
        maxy = foldl1 max $ map v $ M.elems $ frameComponents $ frameHeader s
        threed = resampled `deepseq` V.fromList $ map (\ k -> resampled M.! k) $ L.sort $ M.keys m
        rgb = threed `deepseq` ycbcr2rgb threed
