module Data.JPEG.JFIF where

import Data.Ratio
import Data.Word
import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Generic.Base as GB

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

ycbcr2rgb :: (GB.Vector v c, Integral t, Integral c) => V.Vector (V.Vector (v c)) -> V.Vector (V.Vector (t, t, t))
ycbcr2rgb components = (V.zipWith3 f) y cb cr
  where y = components V.! 0
        cb = components V.! 1
        cr = components V.! 2
        f a b c = V.zipWith3 helper (U.convert a) (U.convert b) (U.convert c)
        helper y' cb' cr' = (back r, back g, back b)
          where y = fromIntegral y'
                cb = fromIntegral cb'
                cr = fromIntegral cr'
                back = floor . clamp 0 255
                r = y                        + 1.402   * (cr - 128)
                g = y - 0.34414 * (cb - 128) - 0.71414 * (cr - 128)
                b = y + 1.772   * (cb - 128)

convertJFIFImage :: JPEGState -> M.Map Word8 (V.Vector (U.Vector Int)) -> V.Vector (V.Vector (Int, Int, Int))
convertJFIFImage s m = ycbcr2rgb threed
  where order = L.sort $ M.keys m
        resampled = M.mapWithKey (\ k v -> resample (xratio k) (yratio k) v) m
        xratio c = (fromIntegral $ h $ (frameComponents $ frameHeader s) M.! c) % (fromIntegral maxx)
        yratio c = (fromIntegral $ v $ (frameComponents $ frameHeader s) M.! c) % (fromIntegral maxy)
        maxx = foldl1 max $ map h $ M.elems $ frameComponents $ frameHeader s
        maxy = foldl1 max $ map v $ M.elems $ frameComponents $ frameHeader s
        threed = V.fromList $ map (\ k -> resampled M.! k) $ L.sort $ M.keys m
