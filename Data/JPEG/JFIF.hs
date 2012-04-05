module Data.JPEG.JFIF where

import Data.Ratio
import Data.Word
import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified Data.List as L

import Data.JPEG.JPEGState
import Data.JPEG.Util

isJFIF :: JPEGState -> Bool
isJFIF _ = True
{-
isJFIF s = not $ null jfifs
  where app0s = [snd v | v <- applicationData s, fst v == 0]
        jfifs = filter (\ bs -> BS.take 5 bs == BS.pack [0x4A, 0x46, 0x49, 0x46, 0x00]) $ app0s
-}

resample :: Integral a => Rational -> Rational -> [[a]] -> [[a]]
resample xfrac yfrac values = map (resample1 $ inc xfrac) $ resample1 (inc yfrac) values

inc frac = helper $ iterate (+ frac) 0
  where helper (x : l@(x' : xs))
          | floor x < floor x' = True : helper l
          | otherwise = False : helper l

resample1 :: [Bool] -> [a] -> [a]
resample1 _ [] = []
resample1 (c : cs) l@(x : xs)
  | c = x : resample1 cs xs
  | otherwise = x : resample1 cs l

ycbcr2rgb :: (Integral a, Integral b) => [[[a]]] -> [[(b, b, b)]]
ycbcr2rgb (y : cb : cr : _) = (zipWith3 (zipWith3 helper)) y cb cr
  where helper y' cb' cr' = (back r, back g, back b)
          where y = fromIntegral y'
                cb = fromIntegral cb'
                cr = fromIntegral cr'
                back = floor . clamp 0 255
                r = y                        + 1.402   * (cr - 128)
                g = y - 0.34414 * (cb - 128) - 0.71414 * (cr - 128)
                b = y + 1.772   * (cb - 128)

convertJFIFImage :: (JPEGState, M.Map Word8 [[Word8]]) -> [[(Word8, Word8, Word8)]]
convertJFIFImage (s, m) = ycbcr2rgb threed
  where order = L.sort $ M.keys m
        resampled = M.mapWithKey (\ k v -> resample (xratio k) (yratio k) v) m
        xratio c = (fromIntegral $ h $ (frameComponents $ frameHeader s) M.! c) % (fromIntegral maxx)
        yratio c = (fromIntegral $ v $ (frameComponents $ frameHeader s) M.! c) % (fromIntegral maxy)
        maxx = foldl1 max $ map h $ M.elems $ frameComponents $ frameHeader s
        maxy = foldl1 max $ map v $ M.elems $ frameComponents $ frameHeader s
        threed = map (\ k -> resampled M.! k) $ L.sort $ M.keys m
