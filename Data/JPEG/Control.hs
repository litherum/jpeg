module Data.JPEG.Control where

import Control.Applicative
import Control.Monad.State
import Data.Attoparsec
import Data.Attoparsec.Binary
import Data.Compression.Huffman
import Data.Default (def)
import Data.Word
import qualified Data.Map as M
import qualified Data.List as L
import Debug.Trace (trace)

import Data.JPEG.JPEGState
import Data.JPEG.Markers
import Data.JPEG.SequentialDCT
import Data.JPEG.Util

-- E.2.5
decodeMCU :: (Integral a, Integral b) => [(Word8, Word8, HuffmanTree Word8, HuffmanTree Word8, [a])] -> StateT BitState Parser [[[b]]]
decodeMCU info = helper info []
  where helper [] l = return $ reverse l
        helper ((count, component, dctree, actree, dequantization_table) : t) l = do
          data_unit <- sequence $ replicate (fromIntegral count) $ decodeDataUnit component dctree actree dequantization_table
          helper t $ data_unit : l

-- E.2.4
decodeRestartInterval :: (Integral a, Integral b) => [(Word8, Word8, HuffmanTree Word8, HuffmanTree Word8, [a])] -> Parser [[[[b]]]]
decodeRestartInterval info = do
  first_word8 <- anyWord8
  o <- helper (8, first_word8, M.fromList $ map (\ (_, c, _, _, _) -> (c, 0)) info) []
  return $ reverse o
  where helper s l = do
          (mcu, s') <- runStateT (decodeMCU info) s
          (helper s' $ mcu : l) <|> (many0 (notWord8 0xFF) >> (return $ mcu : l))

parseSingleScanComponent :: Integral a => JPEGState -> ScanComponent -> StateT JPEGState Parser (M.Map Word8 [[a]])
parseSingleScanComponent s component = do
  interval1 <- lift $ decodeRestartInterval parse_array
  intervals <- lift $ ((decodeIntervals parse_array interval1) <|> (return interval1))
  return $ M.singleton (cs component) $ map (head . head) intervals
  where parse_array = [( 1
                       , cs component
                       , (fst $ huffmanTrees s) M.! (td component)
                       , (snd $ huffmanTrees s) M.! (ta component)
                       , (quantizationTables s) M.! (tq $ (frameComponents $ frameHeader s) M.! (cs component))
                       )]

decodeIntervals :: (Integral a, Integral b) => [(Word8, Word8, HuffmanTree Word8, HuffmanTree Word8, [a])] -> [[[[b]]]] -> Parser [[[[b]]]]
decodeIntervals parse_array l = do
  n <- parseRST
  when (n /= (fromIntegral $ length l) `mod` 8) $ fail "Invalid RST value"
  interval <- decodeRestartInterval parse_array
  (decodeIntervals parse_array (interval ++ l)) <|> return l
  
parseMultipleScanComponents :: Integral a => JPEGState -> [ScanComponent] -> StateT JPEGState Parser (M.Map Word8 [[a]])
parseMultipleScanComponents s components = do
  interval1 <- lift $ decodeRestartInterval $ f components s
  intervals <- lift $ ((decodeIntervals (f components s) interval1) <|> (return interval1))
  let componentized = componentize intervals
  let o = M.fromList $ zipWith (\ sc cluster_list -> rasterize sc cluster_list) components componentized
  return o
  where frame_header = frameHeader s
        f [] _ = []
        f (c : t) s = ( replication
                      , cs c
                      , (fst $ huffmanTrees s) M.! (td c)
                      , (snd $ huffmanTrees s) M.! (ta c)
                      , (quantizationTables s) M.! (tq frame_component)
                      ) : (f t s)
          where replication = (h frame_component) * (v frame_component)
                frame_component = (frameComponents $ frameHeader s) M.! (cs c)
        rasterize sc cluster_list = (cs sc, rearrange myWidth myHeight (width_in_clusters * cluster_width) block_order)
          where block_order = blockOrder width_in_clusters cluster_width cluster_height cluster_list
                width_in_clusters = myWidth `roundUp` (cluster_width * 8)
                cluster_width = fromIntegral $ h ((frameComponents frame_header) M.! (cs sc))
                cluster_height = fromIntegral $ v ((frameComponents frame_header) M.! (cs sc))
                myWidth = ((fromIntegral $ x frame_header) * cluster_width) `roundUp` max_x
                myHeight = ((fromIntegral $ y frame_header) * cluster_height) `roundUp` max_y
                max_x = fromIntegral $ foldl1 max $ map h $ M.elems $ frameComponents frame_header
                max_y = fromIntegral $ foldl1 max $ map v $ M.elems $ frameComponents frame_header

decodeScan :: Integral a => StateT JPEGState Parser (M.Map Word8 [[a]])
decodeScan = do
  s <- get
  s' <- lift $ parseTablesMisc s
  put s'
  scan_header <- lift $ parseScanHeader
  if length (scanComponents scan_header) == 1
    then parseSingleScanComponent s' $ head $ scanComponents scan_header
    else parseMultipleScanComponents s' $ scanComponents scan_header

decodeFrame :: Integral a => Parser (M.Map Word8 [[a]])
decodeFrame = do
  s <- parseTablesMisc def
  frame_header <- parseFrameHeader
  when (n frame_header /= 0 && n frame_header /= 1) $ fail "Unsupported frame!"
  (first_scan, s') <- runStateT decodeScan $ s {frameHeader = frame_header}
  y' <- parseDNLSegment <|> (return $ y $ frameHeader s')
  let s'' = s' {frameHeader = (frameHeader s') {y = y'}}
  scans <- parseRemainingScans $ s''
  return $ M.union first_scan scans
  where parseRemainingScans s = (do
          (o, s') <- runStateT decodeScan s
          os <- parseRemainingScans s'
          return $ M.union o os) <|> return M.empty

decodeJPEG :: Integral a => Parser (M.Map Word8 [[a]])
decodeJPEG = do
  parseSOI
  o <- decodeFrame
  parseEOI
  return o

