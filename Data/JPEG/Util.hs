module Data.JPEG.Util where

import Control.Applicative
import Control.DeepSeq
import Data.Bits
import Data.Word
import Data.Attoparsec
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Control.DeepSeq
import Debug.Trace (trace)

import Data.JPEG.Instances

parseNibbles :: Parser (Word8, Word8)
parseNibbles = do
  w <- anyWord8
  return $ breakWord8 w

breakWord8 :: Word8 -> (Word8, Word8)
breakWord8 w = (w `shiftR` 4, w .&. 0xF)

many0 :: Alternative f => f a -> f [a]
many0 x = many1 x <|> pure []

concatVectors :: V.Vector (V.Vector a) -> V.Vector a
concatVectors = V.foldl (V.++) V.empty 

concatVectors' :: U.Unbox a => V.Vector (U.Vector a) -> U.Vector a
concatVectors' v = U.generate (itemLength * V.length v) f
  where itemLength = U.length $ V.head v
        f i = (v V.! b) U.! a
          where a = i `mod` itemLength
                b = i `div` itemLength

blockOrder :: Int -> Int -> Int -> [[a]] -> [[a]]
blockOrder width_in_clusters cluster_width cluster_height cluster_order = concat $ map (\ clusterrow -> zipWith f (replicate cluster_height clusterrow) [0, cluster_width ..]) matrix
  where matrix = matrixHelper cluster_order
          where matrixHelper [] = []
                matrixHelper cluster_order = L.take width_in_clusters cluster_order : matrixHelper (L.drop width_in_clusters cluster_order)
        f clusterrow i = concat $ map ((L.take cluster_width) . (L.drop i)) clusterrow

blockOrder' :: Int -> Int -> Int -> V.Vector (V.Vector a) -> V.Vector (V.Vector a)
blockOrder' width_in_clusters cluster_width cluster_height cluster_order = concatVectors $ V.map (\ clusterrow -> V.zipWith f (V.replicate cluster_height clusterrow) (V.fromList $ L.take cluster_height [0, cluster_width ..])) matrix
  where matrix = matrixHelper cluster_order
          where matrixHelper cluster_order
                  | V.null cluster_order = V.empty
                  | otherwise = V.cons (V.take width_in_clusters cluster_order) $ matrixHelper (V.drop width_in_clusters cluster_order)
        f clusterrow i = concatVectors $ V.map ((V.take cluster_width) . (V.drop i)) clusterrow

reverseBlockOrder :: NFData a => Int -> Int -> Int -> Int -> V.Vector (V.Vector a) -> V.Vector (V.Vector a)
reverseBlockOrder image_width_in_blocks image_height_in_blocks cluster_width cluster_height block_order = trimmed `deepseq` helper trimmed
  where trimmed = V.take image_height_in_blocks $ V.map (V.take image_width_in_blocks) block_order
        helper mat
          | V.null mat = V.empty
          | otherwise = rowHelper (V.take cluster_height mat) V.++ helper (V.drop cluster_height mat)
          where rowHelper l
                  | V.null $ V.head l = V.empty
                  | otherwise = (concatVectors $ V.map (V.take cluster_width) l) `V.cons` (rowHelper $ V.map (V.drop cluster_width) l)

indices :: [(Int, Int)]
indices = (concat $ map (\ x -> map (\ y -> (y, x-y)) (if x `mod` 2 == 0 then [0..x] else [x,x-1..0])) [0..7]) ++
          (concat $ map (\ x -> map (\ y -> (x-y, y)) (if x `mod` 2 == 0 then [7,6..x-7] else [x-7..7])) [8..15])

indicesV :: U.Vector Int
indicesV = v `deepseq` v
  where v = U.fromList $ map (\ (x, y) -> x * 8 + y) indices

indices' :: Int -> Int -> Int
indices' x y = m `deepseq` (m M.! (x, y))
  where m = foldl (\ m (v, i) -> M.insert v i m) M.empty $ zip indices [0..]

deZigZagIndices :: U.Vector Int
deZigZagIndices = U.fromList [0,2,3,9,10,20,21,35,1,4,8,11,19,22,34,36,5,7,12,18,23,33,37,48,6,13,17,24,32,38,47,49,14,16,25,31,39,46,50,57,15,26,30,40,45,51,56,58,27,29,41,44,52,55,59,62,28,42,43,53,54,60,61,63]

deZigZagBlockIndices :: V.Vector (U.Vector Int)
deZigZagBlockIndices = V.fromList [ U.fromList [0,1,5,6,14,15,27,28]
                                  , U.fromList [2,4,7,13,16,26,29,42]
                                  , U.fromList [3,8,12,17,25,30,41,43]
                                  , U.fromList [9,11,18,24,31,40,44,53]
                                  , U.fromList [10,19,23,32,39,45,52,54]
                                  , U.fromList [20,22,33,38,46,51,55,60]
                                  , U.fromList [21,34,37,47,50,56,59,61]
                                  , U.fromList [35,36,48,49,57,58,62,63]
                                  ]

deZigZag :: U.Unbox a => U.Vector a -> [U.Vector a]
deZigZag block = map (\ y -> U.map (\ x -> block U.! (indices' x y)) $ U.fromList [0..7]) [0..7]

deZigZag' :: U.Unbox a => U.Vector a -> V.Vector (U.Vector a)
--deZigZag' block = V.map (\ y -> U.map (\ x -> block U.! (indices' x y)) $ U.fromList [0..7]) $ V.fromList [0..7]
deZigZag' block = V.map (U.map (block U.!)) deZigZagBlockIndices

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

componentize :: [[a]] -> [[a]]
componentize ([] : _) = []
componentize l = (map head l) : (componentize $ map tail l)

componentize' :: V.Vector (V.Vector a) -> V.Vector (V.Vector a)
componentize' v
  | V.null $ V.head v = V.empty
  | otherwise = V.cons (V.map V.head v) $ componentize' $ V.map V.tail v

batches :: [[[a]]] -> [[a]]
batches ([] : _) = []
batches l = (concat $ map head l) : (batches $ map tail l)

batches' :: U.Unbox a => [[U.Vector a]] -> [U.Vector a]
batches' ([] : _) = []
batches' l = (U.concat $ map head l) : (batches' $ map tail l)

batches'' :: U.Unbox a => V.Vector (V.Vector (U.Vector a)) -> V.Vector (U.Vector a)
batches'' v
  | V.null $ V.head v = V.empty
  | otherwise = V.cons (concatVectors' $ V.map V.head v) $ batches'' $ V.map V.tail v

batches''' :: V.Vector (V.Vector (V.Vector a)) -> V.Vector (V.Vector a)
batches''' v
  | V.null $ V.head v = V.empty
  | otherwise = V.cons (concatVectors $ V.map V.head v) $ batches''' $ V.map V.tail v
