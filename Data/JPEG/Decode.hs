module Data.JPEG.Decode where

import qualified Data.Map as M
import Data.Word

import Data.JPEG.JPEGState
import Data.JPEG.Util

decodeJPEG' :: FrameHeader -> JPEGState -> (M.Map Word8 [[Int]]) -> (M.Map Word8 [[Int]])
decodeJPEG' frame_header state encoded = rasterized
  where dequantized = M.mapWithKey (\ k l -> map (zipWith (*) (map fromIntegral $ (quantizationTables state) M.! (tq $ (frameComponents frame_header) M.! k))) l) encoded
        idcted = M.map (map ((map ((+ 128) . (clamp 0 255) . floor)) . idct)) dequantized
        rasterized = M.mapWithKey rasterize idcted
        rasterize cs blocks = rearrange my_width my_height (width_in_clusters * cluster_width) blocks
          where my_width = ((fromIntegral $ x frame_header) * cluster_width) `roundUp` max_x
                my_height = ((fromIntegral $ y frame_header) * cluster_height) `roundUp` max_y
                cluster_width = fromIntegral $ h $ (frameComponents frame_header) M.! cs
                cluster_height = fromIntegral $ v $ (frameComponents frame_header) M.! cs
                max_x = fromIntegral $ foldl1 max $ map h $ M.elems $ frameComponents frame_header
                max_y = fromIntegral $ foldl1 max $ map v $ M.elems $ frameComponents frame_header
                width_in_clusters = my_width `roundUp` (cluster_width * 8)

idct :: [Int] -> [Double]
idct l = map f indices
  where f (x, y) = 0.25 * (sum $ map (\ ((u, v), a) -> g x y u v a) $ zip indices l)
        g x y u v a = (c u) * (c v) * (fromIntegral a) *
                      (cos (((2 * (fromIntegral x) + 1) * (fromIntegral u) * pi)/16)) *
                      (cos (((2 * (fromIntegral y) + 1) * (fromIntegral v) * pi)/16))
        c 0 = 1.0 / (sqrt 2.0)
        c _ = 1.0