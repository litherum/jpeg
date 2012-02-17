module Data.JPEG.Util where

import Data.Bits
import Data.Word
import Control.Applicative
import Data.Attoparsec
import qualified Data.List as L
import qualified Data.Map as M

breakWord8 :: Word8 -> (Word8, Word8)
breakWord8 w = (w `shiftR` 4, w .&. 0xF)

many0 :: Alternative f => f a -> f [a]
many0 x = many1 x <|> pure []

componentize :: [[a]] -> [[a]]
componentize l = (map head l) : (componentize $ map tail l)

blockOrder :: Int -> Int -> Int -> [[a]] -> [a]
blockOrder width_in_clusters cluster_width cluster_height cluster_order = rowed
  where clusterRows [] = []
        clusterRows c = (L.take width_in_clusters c) : (clusterRows $ L.drop width_in_clusters c)
        cluster_rows = clusterRows cluster_order
        replicated = map (replicate cluster_height) cluster_rows
        rowed = concat $ map (concat . zipWith (\ i r -> concat $ map ((L.take cluster_width) . (drop (i * cluster_width))) r) [0..]) replicated

indices :: [(Int, Int)]
indices = (concat $ map (\ x -> map (\ y -> (y, x-y)) (if x `mod` 2 == 0 then [0..x] else [x,x-1..0])) [0..7]) ++
          (concat $ map (\ x -> map (\ y -> (x-y, y)) (if x `mod` 2 == 0 then [7,6..x-7] else [x-7..7])) [8..15])

indices' :: Int -> Int -> Int
indices' x y = m M.! (x, y)
  where m = foldl (\ m (v, i) -> M.insert v i m) M.empty $ zip indices [0..]

roundUp :: Integral a => a -> a -> a
roundUp a b
  | a `mod` b == 0 = a `div` b
  | otherwise = a `div` b + 1

rearrange :: Int -> Int -> [[b]] -> [[b]]
rearrange x' y' blocks = map (\ y -> map (\ x -> (blocks !! blockindex x y) !! indices' (x `mod` 8) (y `mod` 8)) [0..x'-1]) [0..y'-1]
  where width_in_blocks = x' `roundUp` 8
        blockindex u v = width_in_blocks * (v `div` 8) + (u `div` 8)
        
