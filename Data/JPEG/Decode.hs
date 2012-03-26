module Data.JPEG.Decode where

import qualified Data.Map as M
import Data.Word

--
import qualified Data.Vector.Storable.Mutable as M
import qualified Data.Vector.Storable as V
import qualified Data.Vector as Vector
import Foreign.Storable ( Storable )
import Control.DeepSeq
import Control.Monad.Primitive ( PrimState, PrimMonad )
import Control.Monad( forM_ )
import Control.Monad.ST(ST, runST)
import Data.Bits( shiftL, shiftR )
import Data.Int( Int16 )
--

import Data.JPEG.JPEGState
import Data.JPEG.Util

decodeJPEG' :: FrameHeader -> JPEGState -> (M.Map Word8 [[Int]])
decodeJPEG' frame_header state = rasterized
  where encoded = partialData state
        serialized = M.map concat encoded
        dequantized = M.mapWithKey (\ k l -> map (zipWith (*) (map fromIntegral $ (quantizationTables state) M.! (tq $ (frameComponents frame_header) M.! k))) l) serialized
        idcted = M.map (map ((map ((clamp 0 255) . (+ 128))) . myFastIdct)) dequantized
        rasterized = M.mapWithKey rasterize idcted
        rasterize cs blocks = rearrange my_width my_height (width_in_clusters * cluster_width) blocks
          where my_width = ((fromIntegral $ x frame_header) * cluster_width) `roundUp` max_x
                my_height = ((fromIntegral $ y frame_header) * cluster_height) `roundUp` max_y
                cluster_width = fromIntegral $ h $ (frameComponents frame_header) M.! cs
                cluster_height = fromIntegral $ v $ (frameComponents frame_header) M.! cs
                max_x = fromIntegral $ foldl1 max $ map h $ M.elems $ frameComponents frame_header
                max_y = fromIntegral $ foldl1 max $ map v $ M.elems $ frameComponents frame_header
                width_in_clusters = my_width `roundUp` (cluster_width * 8)

referenceidct :: [Int] -> [Int]
referenceidct l = map (floor . f) indices
  where f (x, y) = 0.25 * (sum $ map (\ ((u, v), a) -> g x y u v a) $ zip indices l)
        g x y u v a = (c u) * (c v) * (fromIntegral a) *
                      (cos (((2 * (fromIntegral x) + 1) * (fromIntegral u) * pi)/16)) *
                      (cos (((2 * (fromIntegral y) + 1) * (fromIntegral v) * pi)/16))
        c 0 = 1.0 / (sqrt 2.0)
        c _ = 1.0

----------------------------------------------------------------------------------------------------------
-- Taken from Juicy Pixels

type MutableMacroBlock s a = M.STVector s a

{-# INLINE (!!!) #-}
(!!!) :: (Storable e) => V.Vector e -> Int -> e
(!!!) = V.unsafeIndex

{-# INLINE (.!!!.) #-}
(.!!!.) :: (PrimMonad m, Storable a)
        => M.STVector (PrimState m) a -> Int -> m a
(.!!!.) = M.unsafeRead

{-# INLINE (.<-.) #-}
(.<-.) :: (PrimMonad m, Storable a)
       => M.STVector (PrimState m) a -> Int -> a -> m ()
(.<-.) = M.unsafeWrite

iclip :: V.Vector Int16
iclip = V.fromListN 1024 [ val i| i <- [(-512) .. 511] ]
    where val i | i < (-256) = -256
                | i > 255    =  255
                | otherwise  =  i

{-# INLINE clip #-}
clip :: Int -> Int16
clip i = iclip !!! (i + 512)

{-# INLINE (.<<.) #-}
{-# INLINE (.>>.) #-}
(.<<.), (.>>.) :: Int -> Int -> Int
(.<<.) = shiftL
(.>>.) = shiftR

data IDctStage = IDctStage { 
        x0 :: {-# UNPACK #-} !Int,
        x1 :: {-# UNPACK #-} !Int,
        x2 :: {-# UNPACK #-} !Int,
        x3 :: {-# UNPACK #-} !Int,
        x4 :: {-# UNPACK #-} !Int,
        x5 :: {-# UNPACK #-} !Int,
        x6 :: {-# UNPACK #-} !Int,
        x7 :: {-# UNPACK #-} !Int,
        x8 :: {-# UNPACK #-} !Int
    }

w1, w2, w3, w5, w6, w7 :: Int
w1 = 2841 -- 2048*sqrt(2)*cos(1*pi/16)
w2 = 2676 -- 2048*sqrt(2)*cos(2*pi/16)
w3 = 2408 -- 2048*sqrt(2)*cos(3*pi/16)
w5 = 1609 -- 2048*sqrt(2)*cos(5*pi/16)
w6 = 1108 -- 2048*sqrt(2)*cos(6*pi/16)
w7 = 565  -- 2048*sqrt(2)*cos(7*pi/16)

-- row (horizontal) IDCT
--
--           7                       pi         1
-- dst[k] = sum c[l] * src[l] * cos( -- * ( k + - ) * l )
--          l=0                      8          2
--
-- where: c[0]    = 128
--        c[1..7] = 128*sqrt(2)
idctRow :: MutableMacroBlock s Int16 -> Int ->  ST s ()
idctRow blk idx = do
  xx0 <- blk .!!!. (0 + idx)
  xx1 <- blk .!!!. (4 + idx) 
  xx2 <- blk .!!!. (6 + idx)
  xx3 <- blk .!!!. (2 + idx)
  xx4 <- blk .!!!. (1 + idx)
  xx5 <- blk .!!!. (7 + idx)
  xx6 <- blk .!!!. (5 + idx)
  xx7 <- blk .!!!. (3 + idx)
  let initialState = IDctStage { x0 = (fromIntegral xx0 .<<. 11) + 128
                               , x1 =  fromIntegral xx1 .<<. 11
                               , x2 =  fromIntegral xx2
                               , x3 =  fromIntegral xx3
                               , x4 =  fromIntegral xx4
                               , x5 =  fromIntegral xx5
                               , x6 =  fromIntegral xx6
                               , x7 =  fromIntegral xx7
                               , x8 = 0
                               } 

      firstStage c = c { x4 = x8'  + (w1 - w7) * x4 c
                       , x5 = x8'  - (w1 + w7) * x5 c
                       , x6 = x8'' - (w3 - w5) * x6 c
                       , x7 = x8'' - (w3 + w5) * x7 c
                       , x8 = x8''
                       }
          where x8' = w7 * (x4 c + x5 c)
                x8'' = w3 * (x6 c + x7 c)

      secondStage c = c { x0 = x0 c - x1 c
                        , x8 = x0 c + x1 c 
                        , x1 = x1''
                        , x2 = x1' - (w2 + w6) * x2 c
                        , x3 = x1' + (w2 - w6) * x3 c
                        , x4 = x4 c - x6 c
                        , x6 = x5 c + x7 c
                        , x5 = x5 c - x7 c
                        }
            where x1'  = w6 * (x3 c + x2 c)
                  x1'' = x4 c + x6 c

      thirdStage c = c { x7 = x8 c + x3 c
                       , x8 = x8 c - x3 c
                       , x3 = x0 c + x2 c
                       , x0 = x0 c - x2 c
                       , x2 = (181 * (x4 c + x5 c) + 128) .>>. 8
                       , x4 = (181 * (x4 c - x5 c) + 128) .>>. 8
                       }
      scaled c = c { x0 = (x7 c + x1 c) .>>. 8
                   , x1 = (x3 c + x2 c) .>>. 8
                   , x2 = (x0 c + x4 c) .>>. 8
                   , x3 = (x8 c + x6 c) .>>. 8
                   , x4 = (x8 c - x6 c) .>>. 8
                   , x5 = (x0 c - x4 c) .>>. 8
                   , x6 = (x3 c - x2 c) .>>. 8
                   , x7 = (x7 c - x1 c) .>>. 8
                   }
      transformed = scaled . thirdStage . secondStage $ firstStage initialState

  (blk .<-. (0 + idx)) . fromIntegral $ x0 transformed
  (blk .<-. (1 + idx)) . fromIntegral $ x1 transformed
  (blk .<-. (2 + idx)) . fromIntegral $ x2 transformed
  (blk .<-. (3 + idx)) . fromIntegral $ x3 transformed
  (blk .<-. (4 + idx)) . fromIntegral $ x4 transformed
  (blk .<-. (5 + idx)) . fromIntegral $ x5 transformed
  (blk .<-. (6 + idx)) . fromIntegral $ x6 transformed
  (blk .<-. (7 + idx)) . fromIntegral $ x7 transformed

-- column (vertical) IDCT
--
--             7                         pi         1
-- dst[8*k] = sum c[l] * src[8*l] * cos( -- * ( k + - ) * l )
--            l=0                        8          2
--
-- where: c[0]    = 1/1024
--        c[1..7] = (1/1024)*sqrt(2)
--
idctCol :: MutableMacroBlock s Int16 -> Int -> ST s ()
idctCol blk idx = do
  xx0 <- blk .!!!. (    0 + idx)
  xx1 <- blk .!!!. (8 * 4 + idx)
  xx2 <- blk .!!!. (8 * 6 + idx)
  xx3 <- blk .!!!. (8 * 2 + idx)
  xx4 <- blk .!!!. (8     + idx)
  xx5 <- blk .!!!. (8 * 7 + idx)
  xx6 <- blk .!!!. (8 * 5 + idx)
  xx7 <- blk .!!!. (8 * 3 + idx)
  let initialState = IDctStage { x0 = (fromIntegral xx0 .<<. 8) + 8192
                               , x1 =  fromIntegral xx1 .<<. 8
                               , x2 =  fromIntegral xx2
                               , x3 =  fromIntegral xx3
                               , x4 =  fromIntegral xx4
                               , x5 =  fromIntegral xx5
                               , x6 =  fromIntegral xx6
                               , x7 =  fromIntegral xx7
                               , x8 = 0
                               } 
      firstStage c = c { x4 = (x8'  + (w1 - w7) * x4 c) .>>. 3
                       , x5 = (x8'  - (w1 + w7) * x5 c) .>>. 3
                       , x6 = (x8'' - (w3 - w5) * x6 c) .>>. 3
                       , x7 = (x8'' - (w3 + w5) * x7 c) .>>. 3
                       , x8 = x8''
                       }
          where x8'  = w7 * (x4 c + x5 c) + 4
                x8'' = w3 * (x6 c + x7 c) + 4

      secondStage c = c { x8 = x0 c + x1 c
                        , x0 = x0 c - x1 c
                        , x2 = (x1' - (w2 + w6) * x2 c) .>>. 3
                        , x3 = (x1' + (w2 - w6) * x3 c) .>>. 3
                        , x4 = x4 c - x6 c
                        , x1 = x1''
                        , x6 = x5 c + x7 c
                        , x5 = x5 c - x7 c
                        }
          where x1'  = w6 * (x3 c + x2 c) + 4
                x1'' = x4 c + x6 c
  
      thirdStage c = c { x7 = x8 c + x3 c
                       , x8 = x8 c - x3 c
                       , x3 = x0 c + x2 c
                       , x0 = x0 c - x2 c
                       , x2 = (181 * (x4 c + x5 c) + 128) .>>. 8
                       , x4 = (181 * (x4 c - x5 c) + 128) .>>. 8
                       }

      f = thirdStage . secondStage $ firstStage initialState
  (blk .<-. (idx + 8*0)) . clip $ (x7 f + x1 f) .>>. 14
  (blk .<-. (idx + 8  )) . clip $ (x3 f + x2 f) .>>. 14
  (blk .<-. (idx + 8*2)) . clip $ (x0 f + x4 f) .>>. 14
  (blk .<-. (idx + 8*3)) . clip $ (x8 f + x6 f) .>>. 14
  (blk .<-. (idx + 8*4)) . clip $ (x8 f - x6 f) .>>. 14
  (blk .<-. (idx + 8*5)) . clip $ (x0 f - x4 f) .>>. 14
  (blk .<-. (idx + 8*6)) . clip $ (x3 f - x2 f) .>>. 14
  (blk .<-. (idx + 8*7)) . clip $ (x7 f - x1 f) .>>. 14


{-# INLINE fastIdct #-}
-- | Algorithm to call to perform an IDCT, return the same
-- block that the one given as input.
fastIdct :: MutableMacroBlock s Int16
         -> ST s (MutableMacroBlock s Int16)
fastIdct block = do
    forM_ [0..7] (\i -> idctRow block (8 * i))
    forM_ [0..7] (idctCol block)
    return block

myFastIdct :: [Int] -> [Int]
myFastIdct block = runST $ do
  mutable_block <- M.new 64
  mapM_ (\ (index, value) -> M.write mutable_block index value) $ zip [0..] dezigzagged
  output <- fastIdct mutable_block
  out <- mapM (M.read output) $ map (\ (x, y) -> x * 8 + y) indices
  return $ map fromIntegral out
  where dezigzagged = [fromIntegral $ vec Vector.! (indices' x y) | x <- [0..7], y <- [0..7]]
        vec = block `deepseq` Vector.fromList block
