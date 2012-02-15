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
decodeMCU :: (Integral a, Integral b) => [(HuffmanTree Word8, HuffmanTree Word8, [a])] -> StateT BitState Parser [[b]]
decodeMCU info = helper info []
  where helper [] l = return $ reverse l
        helper ((dctree, actree, dequantization_table) : t) l = do
          data_unit <- decodeDataUnit dctree actree dequantization_table
          helper t $ data_unit : l

-- E.2.4
decodeRestartInterval :: (Integral a, Integral b) => [(HuffmanTree Word8, HuffmanTree Word8, [a])] -> Parser [[[b]]]
decodeRestartInterval info = do
  first_word8 <- anyWord8
  o <- helper (8, first_word8, 0) []
  return $ reverse o
  where helper s l = do
          (mcu, s') <- runStateT (decodeMCU info) s
          (helper s' $ mcu : l) <|> (many0 (notWord8 0xFF) >> return l)

--decodeScan :: StateT JPEGState Parser [[[[Int]]]]
decodeScan = do
  s <- get
  s' <- lift $ parseTablesMisc s
  put s'
  scan_header <- lift $ parseScanHeader
  interval1 <- trace (show scan_header) $ lift $ decodeRestartInterval $ f (scanComponents scan_header) s'
  intervals <- lift $ decodeIntervals (scanComponents scan_header) s' [] <|> return []
  return $ interval1 : intervals
  where decodeIntervals scan_components s l = do
          n <- parseRST
          when (n /= (length l) `mod` 8) $ fail "Invalid RST value"
          interval <- decodeRestartInterval $ f scan_components s
          (decodeIntervals scan_components s (interval : l)) <|> return l
        f components s
          | length components < 1 = fail "Scan has less than 1 component"
          | length components == 1 = [( (fst $ huffmanTrees s) M.! (td $ head components)
                                      , (snd $ huffmanTrees s) M.! (ta $ head components)
                                      , (quantizationTables s) M.! (tq $ (frameComponents $ frameHeader s) M.! (cs $ head components))
                                      )]
          | otherwise = g components s
        g [] _ = []
        g (c : t) s = (L.replicate (fromIntegral replication)
                         ( (fst $ huffmanTrees s) M.! (td c)
                         , (snd $ huffmanTrees s) M.! (ta c)
                         , (quantizationTables s) M.! (tq frame_component)
                         )) ++ (g t s)
          where replication = (h frame_component) * (v frame_component)
                frame_component = (frameComponents $ frameHeader s) M.! (cs c)

--decodeFrame :: Parser [[[[[Int]]]]]
decodeFrame = do
  s <- parseTablesMisc def
  frame_header <- parseFrameHeader
  (first_scan, s') <- trace (show frame_header) $ runStateT decodeScan $ s {frameHeader = frame_header}
  y' <- parseDNLSegment <|> (return $ y $ frameHeader s')
  scans <- parseRemainingScans $ s' {frameHeader = (frameHeader s') {y = y'}}
  return $ first_scan : scans
  where parseRemainingScans s = (do
          (o, s') <- runStateT decodeScan s
          os <- parseRemainingScans s'
          return $ o : os) <|> return []

decodeJPEG = do
  parseSOI
  o <- decodeFrame
  parseEOI
  return o
