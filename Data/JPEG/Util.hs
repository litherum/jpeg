module Data.JPEG.Util where

import Data.Bits
import Data.Word
import Control.Applicative
import Data.Attoparsec
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Vector as V
import Control.DeepSeq
import Debug.Trace (trace)

breakWord8 :: Word8 -> (Word8, Word8)
breakWord8 w = (w `shiftR` 4, w .&. 0xF)

many0 :: Alternative f => f a -> f [a]
many0 x = many1 x <|> pure []

componentize :: [[a]] -> [[a]]
componentize ([] : _) = []
componentize l = (map head l) : (componentize $ map tail l)

blockOrder :: Int -> Int -> Int -> [[a]] -> [a]
blockOrder width_in_clusters cluster_width cluster_height cluster_order = map f [0 .. cluster_width * cluster_height * length cluster_order - 1]
  where f index = (cluster_order_v V.! index_cluster) V.! cluster_index
          where xblock = index `mod` (width_in_clusters * cluster_width)
                yblock = index `div` (width_in_clusters * cluster_width)
                xcluster = xblock `div` cluster_width
                ycluster = yblock `div` cluster_height
                index_cluster = ycluster * width_in_clusters + xcluster
                clusterx = xblock `mod` cluster_width
                clustery = yblock `mod` cluster_height
                cluster_index = clustery * cluster_width + clusterx
                
                
        cluster_order_v = V.fromList $ map V.fromList cluster_order

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

rearrange :: Int -> Int -> Int -> [[b]] -> [[b]]
rearrange x' y' width_in_blocks blocks = map (\ y -> map (\ x -> (blocks_v V.! (blockindex x y)) V.! indices' (x `mod` 8) (y `mod` 8)) [0..x'-1]) [0..y'-1]
  where blockindex u v = width_in_blocks * (v `div` 8) + (u `div` 8)
        blocks_v = V.fromList $ map V.fromList blocks

clamp :: Ord c => c -> c -> c -> c
clamp l h = max l . min h
