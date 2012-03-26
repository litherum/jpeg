module Data.JPEG.Util where

import Control.Applicative
import Control.DeepSeq
import Data.Bits
import Data.Word
import Data.Attoparsec
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Vector as V
import Control.DeepSeq
import Debug.Trace (trace)

parseNibbles :: Parser (Word8, Word8)
parseNibbles = do
  w <- anyWord8
  return $ breakWord8 w

breakWord8 :: Word8 -> (Word8, Word8)
breakWord8 w = (w `shiftR` 4, w .&. 0xF)

many0 :: Alternative f => f a -> f [a]
many0 x = many1 x <|> pure []

componentize :: [[a]] -> [[a]]
componentize ([] : _) = []
componentize l = (map head l) : (componentize $ map tail l)

blockOrder :: Int -> Int -> Int -> [[a]] -> [[a]]
blockOrder width_in_clusters cluster_width cluster_height cluster_order = concat $ map (\ clusterrow -> zipWith f (replicate cluster_height clusterrow) [0, cluster_width ..]) matrix
  where matrix = matrixHelper cluster_order
          where matrixHelper [] = []
                matrixHelper cluster_order = L.take width_in_clusters cluster_order : matrixHelper (L.drop width_in_clusters cluster_order)
        f clusterrow i = concat $ map ((L.take cluster_width) . (L.drop i)) clusterrow

reverseBlockOrder :: Int -> Int -> Int -> Int -> [[a]] -> [[a]]
reverseBlockOrder image_width_in_blocks image_height_in_blocks cluster_width cluster_height block_order = helper trimmed
  where trimmed = L.take image_height_in_blocks $ map (L.take image_width_in_blocks) block_order
        helper [] = []
        helper mat = rowHelper (L.take cluster_height mat) ++ helper (L.drop cluster_height mat)
          where rowHelper ([] : _) = []
                rowHelper l = (concat $ map (L.take cluster_width) l) : (rowHelper $ map (L.drop cluster_width) l)

indices :: [(Int, Int)]
indices = (concat $ map (\ x -> map (\ y -> (y, x-y)) (if x `mod` 2 == 0 then [0..x] else [x,x-1..0])) [0..7]) ++
          (concat $ map (\ x -> map (\ y -> (x-y, y)) (if x `mod` 2 == 0 then [7,6..x-7] else [x-7..7])) [8..15])

indices' :: Int -> Int -> Int
indices' x y = m `deepseq` (m M.! (x, y))
  where m = foldl (\ m (v, i) -> M.insert v i m) M.empty $ zip indices [0..]

roundUp :: Integral a => a -> a -> a
roundUp a b
  | a `mod` b == 0 = a `div` b
  | otherwise = (a `div` b) + 1

makeMultipleOf :: Integral a => a -> a -> a
makeMultipleOf a b = (a `roundUp` b) * b

rearrange :: Int -> Int -> Int -> [[b]] -> [[b]]
rearrange x' y' width_in_blocks blocks = map (\ y -> map (\ x -> (blocks_v V.! (blockindex x y)) V.! indices' (x `mod` 8) (y `mod` 8)) [0..x'-1]) [0..y'-1]
  where blockindex u v = width_in_blocks * (v `div` 8) + (u `div` 8)
        blocks_v = V.fromList $ map V.fromList blocks

clamp :: Ord c => c -> c -> c -> c
clamp l h = max l . min h
