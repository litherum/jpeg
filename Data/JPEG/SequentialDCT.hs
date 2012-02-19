module Data.JPEG.SequentialDCT where

import Control.Monad.State
import Data.Attoparsec
import Data.Attoparsec.Binary
import Data.Bits
import Data.Compression.Huffman
import Data.Word
import Debug.Trace (trace)
import qualified Data.Map as M
import Data.Int

import Data.JPEG.Util

-- F.2.2.1
extend :: (Bits a, Ord a) => a -> Int -> a
extend v t
  | v == 0 && t == 0 = 0
  | v < vt = v + (-1 `shiftL` t) + 1
  | otherwise = v
  where vt = 2 ^ (t - 1)

type BitState = (Word8, Word8, M.Map Word8 Int)  -- cnt, b, (c -> pred)

-- F.2.2.5
nextBit :: StateT BitState Parser Word8
nextBit = do
  (cnt, b, pred) <- get
  if cnt == 0
    then do
      b' <- lift anyWord8
      if b' == 0xFF
        then do
          b2 <- lift anyWord8
          if b2 == 0x00
            then out 8 b' pred
            else lift $ fail "Unexpected marker"
        else out 8 b' pred
    else out cnt b pred
  where out cnt b pred = do
          put (cnt - 1, b `shiftL` 1, pred)
          return $ b `shiftR` 7

-- F.2.2.4
receive :: (Bits b, Num a) => a -> StateT BitState Parser b
receive s = helper s 0 0
  where helper s i v
         | i == s = return v
         | otherwise = do
           nb <- nextBit
           helper s (i + 1) ((v `shiftL` 1) + (fromIntegral nb))

-- F.2.2.3
decode :: HuffmanTree a -> StateT BitState Parser a
decode Empty = lift $ fail "Value not in huffman tree"
decode (Leaf x) = return x
decode (Node l r) = do
  nb <- nextBit
  decode $ if nb == 1 then r else l

-- F.2.2.1
diff :: (Integral a, Ord b, Bits b) => HuffmanTree a -> StateT BitState Parser b
diff tree = do
  t <- decode tree
  d <- receive t
  let o = extend d $ fromIntegral t
  return o

-- F.2.2.2
decodeACCoefficients :: (Bits a, Integral a) => HuffmanTree Word8 -> StateT BitState Parser [a]
decodeACCoefficients tree = do
  o <- helper 63 []
  return $ concat $ reverse o
  where helper k zz
         | k < 0 = lift $ fail "AC elements not properly aligned"
         | k == 0 = return zz
         | otherwise = do
           rs <- decode tree
           let (r, s) = breakWord8 rs
           if s == 0
             then if r == 15
               then helper (k - 16) $ replicate 16 0 : zz
               else return $ replicate k 0 : zz
             else do
               o <- receive s
               let o' = extend o $ fromIntegral s
               helper (k - (fromIntegral r) - 1) $ [o'] : (replicate (fromIntegral r) 0) : zz

decodeDataUnit :: (Integral b, Integral a) => Word8 -> HuffmanTree Word8 -> HuffmanTree Word8 -> [a] -> StateT BitState Parser [b]
decodeDataUnit c dctree actree dequantizationtable = do
  (_, _, pred_m) <- get
  d <- diff dctree
  let dc = (pred_m M.! c) + d
  (cnt, b, _) <- get
  put (cnt, b, M.insert c dc pred_m)
  acs <- decodeACCoefficients actree
  return $ map (floor . clamp 0 255 . (+ 128)) $ idct $ zipWith (*) (map fromIntegral dequantizationtable) $ dc : acs

clamp :: Ord c => c -> c -> c -> c
clamp l h = max l . min h

idct :: (Integral a, Floating b) => [a] -> [b]
idct l = map f indices
  where f (x, y) = 0.25 * (sum $ map (\ ((u, v), a) -> g x y u v a) $ zip indices l)
        g x y u v a = (c u) * (c v) * (fromIntegral a) *
                      (cos (((2 * (fromIntegral x) + 1) * (fromIntegral u) * pi)/16)) *
                      (cos (((2 * (fromIntegral y) + 1) * (fromIntegral v) * pi)/16))
        c 0 = 1.0 / (sqrt 2.0)
        c _ = 1.0
