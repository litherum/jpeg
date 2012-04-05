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

import Data.JPEG.DataUnit
import Data.JPEG.Decode
import Data.JPEG.JPEGState
import Data.JPEG.Markers
import Data.JPEG.Util

decodeMCU :: [(Int, Word8, HuffmanTree Word8, HuffmanTree Word8)] -> Word8 -> Word8 -> Word8 -> Word8 -> [[Int]] -> DataUnitFunc -> StateT BitState Parser [[[Int]]]
decodeMCU info ss se ah al existing dataUnitFunc = helper info existing []
  where helper [] _ l = return $ reverse l
        helper ((count, component, dctree, actree) : t) es l = do
          data_unit <- mapM (\ e -> dataUnitFunc e component dctree actree ss se ah al) $ L.take (fromIntegral count) es
          helper t (L.drop (fromIntegral count) es) $ data_unit : l

decodeRestartIntervals :: [(Int, Word8, HuffmanTree Word8, HuffmanTree Word8)] -> Word8 -> Word8 -> Word8 -> Word8 -> Word16 -> [[[Int]]] -> DataUnitFunc -> Parser [[[[Int]]]]
decodeRestartIntervals info ss se ah al ri existing dataUnitFunc = do
  o <- helper 0 (0, 0, M.fromList $ map (\ (_, c, _, _) -> (c, 0)) info, 0) existing []
  return $ reverse o
  where helper c s [] l = return l
        helper c s (e : es) l = do
          s' <- if ri /= 0 && c /= 0 && c `mod` (fromIntegral ri) == 0
            then do  -- Restart interval
              ri' <- parseRST
              when (ri' /= (fromIntegral $ ((c `quot` (fromIntegral ri)) - 1) `mod` 8)) $ trace "Restart interval incorrect" $ fail "Restart interval incorrect"
              return (0, 0, M.fromList $ map (\ (_, c, _, _) -> (c, 0)) info, 0)
            else return s
          (mcu, s'') <- runStateT (decodeMCU info ss se ah al e dataUnitFunc) s'
          (helper (c + 1) s'' es $ mcu : l) <|> (trace ("Failed.") $ (return $ mcu : l))

parseScan :: StateT JPEGState Parser ()
parseScan = do
  s' <- get
  s <- lift $ parseTablesMisc s'
  scan_header <- lift $ parseScanHeader
  trace (show scan_header) $ return ()
  helper s (frameHeader s) scan_header
    where helper s frame_header scan_header = do
          let (data_unit_func, existing) = case (n frame_header, scan_header) of
                (0, _) -> (decodeSequentialDataUnit, repeat (repeat []))
                (1, _) -> (decodeSequentialDataUnit, repeat (repeat []))
                (2, ScanHeader _ 0 0 0 al) -> (decodeDCDataUnit, repeat (repeat []))
                (2, ScanHeader scan_components 0 0 _ _) -> (decodeSubsequentDCScans, batches $ map (breakUp (partialData s)) scan_components)
                (2, ScanHeader scan_components _ _ _ _) -> (decodeACScans, batches $ map (breakUp (partialData s)) scan_components)
          updated <- lift $ decodeRestartIntervals (map (component2Info s) (scanComponents scan_header))
            (ss scan_header) (se scan_header) (ah scan_header) (al scan_header) (restartInterval s) existing data_unit_func
          let pd = foldl1 M.union $ zipWith (apply $ partialData s) (scanComponents scan_header) $ componentize updated
          put s {partialData = foldl (flip M.union) (partialData s) $ zipWith (apply $ partialData s) (scanComponents scan_header) $ componentize updated}
          where max_x = fromIntegral $ foldl1 max $ map h $ M.elems $ frameComponents frame_header
                max_y = fromIntegral $ foldl1 max $ map v $ M.elems $ frameComponents frame_header
                ns' = length $ scanComponents scan_header
                component2Info s (ScanComponent cs td ta) = ( count cs
                                                            , cs
                                                            , M.findWithDefault Empty td $ fst $ huffmanTrees s
                                                            , M.findWithDefault Empty ta $ snd $ huffmanTrees s
                                                            )
                breakUp partial_data (ScanComponent cs _ _) = reverseBlockOrder
                                                                (makeMultipleOf (imageWidthToBlockForComponent cs) $ fakeClusterWidth cs)
                                                                (makeMultipleOf (imageHeightToBlockForComponent cs) $ fakeClusterHeight cs)
                                                                (fakeClusterWidth cs)
                                                                (fakeClusterHeight cs)
                                                                (partial_data M.! cs)
                apply previousBuffer' (ScanComponent cs _ _) updated
                  | M.notMember cs previousBuffer' = M.singleton cs diff
                  | otherwise = M.singleton cs wrap
                  where previousBuffer = previousBuffer' M.! cs
                        diff = blockOrder
                                 ((imageWidthToBlockForComponent cs) `roundUp` (fakeClusterWidth cs))
                                 (fakeClusterWidth cs)
                                 (fakeClusterHeight cs)
                                 updated
                        wrap = zipWith wrapRow diff previousBuffer ++ drop (length diff) previousBuffer
                        wrapRow new old = new ++ (drop (length new) old)
                imageWidthToBlockForComponent cs = (((fromIntegral $ x frame_header) `roundUp` 8) * (clusterWidth cs)) `roundUp` max_x
                imageHeightToBlockForComponent cs = (((fromIntegral $ y frame_header) `roundUp` 8) * (clusterHeight cs)) `roundUp` max_y
                clusterWidth cs = fromIntegral $ h $ (frameComponents frame_header) M.! cs
                clusterHeight cs = fromIntegral $ v $ (frameComponents frame_header) M.! cs
                fakeClusterWidth cs
                  | ns' == 1 = 1
                  | otherwise = clusterWidth cs
                fakeClusterHeight cs
                  | ns' == 1 = 1
                  | otherwise = clusterHeight cs
                count cs = fakeClusterWidth cs * fakeClusterHeight cs

decodeFrame :: Parser (JPEGState, M.Map Word8 [[Int]])
decodeFrame = do
  s <- parseTablesMisc def
  frame_header <- parseFrameHeader
  trace (show frame_header) $ return ()
  s' <- execStateT parseScan $ s { frameHeader = frame_header }
  y' <- parseDNLSegment <|> (return $ y frame_header)
  let frame_header' = frame_header {y = y'}
  s'' <- parseScans $ s' { frameHeader = frame_header { y = y' } }
  return $ (s'', decodeJPEG' s'')
  where parseScans s = (do
          s' <- execStateT parseScan s
          parseScans s') <|> return s

decodeJPEG :: Parser (JPEGState, M.Map Word8 [[Int]])
decodeJPEG = do
  parseSOI
  o <- decodeFrame
  parseEOI
  return o
