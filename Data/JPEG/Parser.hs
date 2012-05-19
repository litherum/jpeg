module Data.JPEG.Parser where

import Control.Applicative
import Control.DeepSeq
import Control.Monad.Identity
import Control.Monad.State
import Data.Attoparsec
import Data.Attoparsec.Binary
import Data.Bits
import Data.Compression.Huffman
import Data.Default
import Data.Word
import Debug.Trace (trace)
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Vector as V
import Data.Int
import qualified Data.Vector.Unboxed as U

import Data.JPEG.DataUnit
import Data.JPEG.JPEGState
import Data.JPEG.Markers
import Data.JPEG.Util

type SingleOrderInfo = (Int, Word8, HuffmanTree Word8, HuffmanTree Word8)
type OrderInfo = V.Vector SingleOrderInfo

gatherComponents :: OrderInfo -> [Word8]
gatherComponents = (map (\ (_, c, _, _) -> c)) . V.toList

component2Info :: FrameHeader -> [ScanComponent] -> JPEGState -> ScanComponent -> SingleOrderInfo
component2Info frame_header scan_components s (ScanComponent cs td ta) = (count, cs, helper td fst, helper ta snd)
  where count = countClusterItems frame_header scan_components cs
        helper t f = M.findWithDefault Empty t $ f $ huffmanTrees s

getInfo :: FrameHeader -> [ScanComponent] -> JPEGState -> OrderInfo
getInfo frame_header scan_components s = V.fromList $ map (component2Info frame_header scan_components s) scan_components

-- returns (max_x, max_y)
maxFrameRatios :: FrameHeader -> (Int, Int)
maxFrameRatios frame_header = (helper h, helper v)
  where helper f = fromIntegral $ foldl1 max $ map f $ M.elems $ frameComponents frame_header

-- returns (x, y)
imageDimensionsToBlock :: FrameHeader -> Word8 -> (Int, Int)
imageDimensionsToBlock frame_header cs = (helper x w max_x, helper y h max_y)
  where helper f d max = (((fromIntegral $ f frame_header) `roundUp` 8) * d) `roundUp` max
        (max_x, max_y) = maxFrameRatios frame_header
        (w, h) = clusterDimensions frame_header cs

-- returns (x, y)
clusterDimensions :: FrameHeader -> Word8 -> (Int, Int)
clusterDimensions frame_header cs = (helper h, helper v)
  where helper f = fromIntegral $ f $ (frameComponents frame_header) M.! cs

-- returns (x, y)
fakeClusterDimensions :: FrameHeader -> [ScanComponent] -> Word8 -> (Int, Int)
fakeClusterDimensions frame_header scan_components cs
  | length scan_components == 1 = (1, 1)
  | otherwise = clusterDimensions frame_header cs

countClusterItems :: FrameHeader -> [ScanComponent] -> Word8 -> Int
countClusterItems frame_header scan_components cs = a * b
  where (a, b) = fakeClusterDimensions frame_header scan_components cs

-----------------------------------------------------------------------------------------------------

decodeMCU :: OrderInfo -> Word8 -> Word8 -> Word8 -> Word8 -> DataUnitFunc -> StateT BitState Parser (V.Vector (V.Vector (U.Vector Int)))
decodeMCU info ss se ah al dataUnitFunc = V.mapM helper info
  where helper :: SingleOrderInfo -> StateT BitState Parser (V.Vector (U.Vector Int))
        helper (count, component, dctree, actree) = V.replicateM count $ dataUnitFunc component dctree actree ss se ah al

decodeUpdateMCU :: OrderInfo -> Word8 -> Word8 -> Word8 -> Word8 -> UpdateDataUnitFunc -> V.Vector (V.Vector (U.Vector Int)) -> StateT BitState Parser (V.Vector (V.Vector (U.Vector Int)))
decodeUpdateMCU info ss se ah al dataUnitFunc existing = V.mapM helper $ V.zip info existing
  where helper :: (SingleOrderInfo, V.Vector (U.Vector Int)) -> StateT BitState Parser (V.Vector (U.Vector Int))
        helper ((_, component, dctree, actree), e) = V.mapM (\ e' -> dataUnitFunc e' component dctree actree ss se ah al) e

decodeRestartIntervals :: OrderInfo -> Word8 -> Word8 -> Word8 -> Word8 -> Word16 -> DataUnitFunc -> Parser (V.Vector (V.Vector (V.Vector (U.Vector Int))))
decodeRestartIntervals info ss se ah al ri dataUnitFunc = helper default_bitstate [] >>= return . V.fromList
  where default_diffs = M.fromList $ map (\ c -> (c, 0)) $ gatherComponents info
        default_bitstate = (0, 0, default_diffs, 0)
        helper s l = do
          let c = L.length l
          s' <- if ri /= 0 && c /= 0 && c `mod` (fromIntegral ri) == 0
            then do  -- Restart interval
              ri' <- parseRST
              when (ri' /= (fromIntegral $ ((c `quot` (fromIntegral ri)) - 1) `mod` 8)) $ trace "Restart interval incorrect" $ fail "Restart interval incorrect"
              return default_bitstate
            else return s
          (mcu, s'') <- runStateT (decodeMCU info ss se ah al dataUnitFunc) s'
          let updated = mcu : l
          (helper s'' updated) <|> (trace ("Failed.") $ return $ reverse updated)

decodeUpdateRestartIntervals :: OrderInfo -> Word8 -> Word8 -> Word8 -> Word8 -> Word16 -> UpdateDataUnitFunc -> V.Vector (V.Vector (V.Vector (U.Vector Int))) -> Parser (V.Vector (V.Vector (V.Vector (U.Vector Int))))
decodeUpdateRestartIntervals info ss se ah al ri dataUnitFunc existing = evalStateT (V.sequence rsts) default_bitstate
  where default_diffs = M.fromList $ map (\ c -> (c, 0)) $ gatherComponents info
        default_bitstate = (0, 0, default_diffs, 0)
        states :: V.Vector (StateT BitState Parser (V.Vector (V.Vector (U.Vector Int))))
        states = V.map (decodeUpdateMCU info ss se ah al dataUnitFunc) existing
        rsts :: V.Vector (StateT BitState Parser (V.Vector (V.Vector (U.Vector Int))))
        rsts = V.zipWith parseRSTs states $ V.fromList [0 .. (V.length states) - 1]
          where parseRSTs :: StateT BitState Parser (V.Vector (V.Vector (U.Vector Int))) -> Int -> StateT BitState Parser (V.Vector (V.Vector (U.Vector Int)))
                parseRSTs m i
                  | ri /= 0 && i /= 0 && i `mod` (fromIntegral ri) == 0 = do
                    ri' <- lift $ parseRST
                    when (ri' /= (fromIntegral $ ((i `quot` (fromIntegral ri)) - 1) `mod` 8)) $ trace "Restart interval incorrect" $ fail "Restart interval incorrect"
                    put default_bitstate
                    m
                  | otherwise = m

createNewComponents :: JPEGState -> ScanHeader -> Parser JPEGState
createNewComponents s (ScanHeader scan_components ss se ah al) = do
  raw <- decodeRestartIntervals (getInfo frame_header scan_components s) ss se ah al (restartInterval s) data_unit_func
  return $ s {partialData = V.foldl (\ m (scan_component, d) -> M.insert (cs scan_component) (frame (cs scan_component) d) m) (partialData s) $
    V.zip (V.fromList scan_components) $ componentize' raw}
  where frame_header = frameHeader s
        data_unit_func
          | n' == 0 || n' == 1 = decodeSequentialDataUnit
          | otherwise = decodeDCDataUnit
          where n' = n frame_header
        frame :: Word8 -> V.Vector (V.Vector (U.Vector Int)) -> V.Vector (V.Vector (U.Vector Int))
        frame cs = blockOrder'
                    (image_dimensions_to_block_x `roundUp` fake_cluster_width)
                    fake_cluster_width fake_cluster_height
          where (fake_cluster_width, fake_cluster_height) = fakeClusterDimensions frame_header scan_components cs
                (image_dimensions_to_block_x, _) = imageDimensionsToBlock frame_header cs

updateExistingComponents :: JPEGState -> ScanHeader -> Parser JPEGState
updateExistingComponents s (ScanHeader scan_components ss se ah al) = do
  raw <- decodeUpdateRestartIntervals (getInfo frame_header scan_components s) ss se ah al (restartInterval s) data_unit_func existing
  return $ s {partialData = V.foldl (flip M.union) (partialData s) $ V.zipWith (apply $ partialData s) (V.fromList scan_components) $ componentize' raw}
  where frame_header = frameHeader s
        existing :: V.Vector (V.Vector (V.Vector (U.Vector Int)))
        existing = componentize' $ V.map (breakUp (partialData s)) $ V.fromList scan_components
        breakUp :: M.Map Word8 (V.Vector (V.Vector (U.Vector Int))) -> ScanComponent -> V.Vector (V.Vector (U.Vector Int))
        breakUp partial_data (ScanComponent cs _ _) = reverseBlockOrder
                                                        (makeMultipleOf image_width_to_block $ fake_cluster_width)
                                                        (makeMultipleOf image_height_to_block $ fake_cluster_height)
                                                        fake_cluster_width fake_cluster_height (partial_data M.! cs)
          where (fake_cluster_width, fake_cluster_height) = fakeClusterDimensions frame_header scan_components cs
                (image_width_to_block, image_height_to_block) = imageDimensionsToBlock frame_header cs
        data_unit_func
          | ss == 0 && se == 0 = decodeSubsequentDCScans
          | otherwise = decodeACScans
        apply :: M.Map Word8 (V.Vector (V.Vector (U.Vector Int))) -> ScanComponent -> V.Vector (V.Vector (U.Vector Int)) -> M.Map Word8 (V.Vector (V.Vector (U.Vector Int)))
        apply previousBuffer' (ScanComponent cs _ _) updated = M.singleton cs wrap
          where previousBuffer = previousBuffer' M.! cs
                diff = blockOrder'
                         (image_width_to_block `roundUp` fake_cluster_width)
                         fake_cluster_width fake_cluster_height updated
                wrap = diff `deepseq` (V.zipWith wrapRow diff previousBuffer V.++ V.drop (V.length diff) previousBuffer)
                wrapRow new old = new V.++ (V.drop (V.length new) old)
                (fake_cluster_width, fake_cluster_height) = fakeClusterDimensions frame_header scan_components cs
                (image_width_to_block, image_height_to_block) = imageDimensionsToBlock frame_header cs

parseScan :: StateT JPEGState Parser ()
parseScan = do
  s' <- get
  s <- lift $ parseTablesMisc s'
  scan_header <- lift $ parseScanHeader
  trace (show scan_header) $ return ()
  s'' <- lift $ (decider (n $ frameHeader s) scan_header) s scan_header
  put s''
    where decider n scan_header
            | n == 0 ||
              n == 1 ||
              n == 2 && (ss scan_header == 0 &&
                         se scan_header == 0 &&
                         ah scan_header == 0) = createNewComponents
            | otherwise = updateExistingComponents

decodeFrame :: Parser JPEGState
decodeFrame = do
  s <- parseTablesMisc def
  frame_header <- parseFrameHeader
  trace (show frame_header) $ return ()
  s' <- execStateT parseScan $ s { frameHeader = frame_header }
  y' <- s' `deepseq` (parseDNLSegment <|> (return $ y frame_header))
  let frame_header' = frame_header {y = y'}
  s'' <- parseScans $ s' { frameHeader = frame_header { y = y' } }
  return s''
  where parseScans s = (do
          s' <- execStateT parseScan s
          s' `deepseq` (parseScans s')) <|> return s

decodeJPEG :: Parser JPEGState
decodeJPEG = do
  parseSOI
  o <- decodeFrame
  parseEOI
  return o
