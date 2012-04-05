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

import Data.JPEG.Decode
import Data.JPEG.JPEGState
import Data.JPEG.Markers
import Data.JPEG.Util

-- F.2.2.1
extend :: (Bits a, Ord a) => a -> Int -> a
extend v t
  | v == 0 && t == 0 = 0
  | v < vt = v + (-1 `shiftL` t) + 1
  | otherwise = v
  where vt = 2 ^ (t - 1)

type BitState = (Word8, Word8, M.Map Word8 Int, Int)  -- cnt, b, (c -> pred), eobrun

-- F.2.2.5
nextBit :: StateT BitState Parser Word8
nextBit = do
  (cnt, b, pred, eobrun) <- get
  if cnt == 0
    then do
      b' <- lift anyWord8
      if b' == 0xFF
        then do
          b2 <- lift anyWord8
          if b2 == 0x00
            then out 8 b' pred eobrun
            else trace ("Unexpected marker: " ++ (show b2)) $ lift $ fail "Unexpected marker"
        else out 8 b' pred eobrun
    else out cnt b pred eobrun
  where out cnt b pred eobrun = do
          put (cnt - 1, b `shiftL` 1, pred, eobrun)
          return $ b `shiftR` 7

-- F.2.2.4
receive :: (Bits b, Num a) => a -> StateT BitState Parser b
receive s = helper s 0 0
  where helper s i v
         | i == s = return v
         | otherwise = do
           nb <- nextBit
           helper s (i + 1) ((v `shiftL` 1) + (fromIntegral nb))

-- F.2.2.3
decode :: HuffmanTree a -> StateT BitState Parser a
decode Empty = trace "Value not in huffman tree" $ lift $ fail "Value not in huffman tree"
decode (Leaf x) = return x
decode (Node l r) = do
  nb <- nextBit
  decode $ if nb == 1 then r else l

-- F.2.2.1
diff :: (Num a, Integral a) => Word8 -> HuffmanTree a -> StateT BitState Parser Int
diff c tree = do
  t <- decode tree
  d <- receive t
  (cnt, b, pred_m, eobrun) <- get
  let dc = (pred_m M.! c) + (extend d $ fromIntegral t)
  put (cnt, b, M.insert c dc pred_m, eobrun)
  return dc

type DataUnitFunc = [Int] -> Word8 -> HuffmanTree Word8 -> HuffmanTree Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> StateT BitState Parser [Int]

decodeSequentialDataUnit :: DataUnitFunc
decodeSequentialDataUnit existing c dctree actree 0 63 0 0 = do
  dc <- decodeDCDataUnit existing c dctree actree 0 63 0 0
  ac <- decodeACScans dc c dctree actree 1 63 0 0
  return ac

decodeDCDataUnit :: DataUnitFunc
decodeDCDataUnit _ c tree _ _ _ _ al = do
  dc <- diff c tree
  return $ (dc * (2 ^ al)) : replicate 63 0

decodeSubsequentDCScans :: DataUnitFunc
decodeSubsequentDCScans existing _ _ _ 0 0 ah al = do
  d <- receive (ah - al)
  return $ ((head existing) + (d * (2 ^ al))) : (tail existing)

decodeACScans :: DataUnitFunc
decodeACScans existing _ _ tree ss se ah al = do
  (cnt, b, pred, eobrun) <- get
  if eobrun == 0
    then do
      o <- helper ss middle []
      return $ beginning ++ o ++ end
    else do
      put (cnt, b, pred, eobrun - 1)
      modified <- appendBitToEach al middle (-1)
      return $ beginning ++ modified ++ end
  where beginning = L.take (fromIntegral ss) existing
        middle = L.take (fromIntegral $ se - ss + 1) $ L.drop (fromIntegral ss) existing
        end = L.drop (fromIntegral $ se + 1) existing
        helper k rzz lzz
         | k > se + 1 = trace "Successive elements not properly aligned" $ lift $ fail "Successive elements not properly aligned"
         | k == se + 1 && not (null rzz) = trace "rzz not null!" $ lift $ fail "rzz not null!"
         | k == se + 1 = return $ concat $ reverse lzz
         | otherwise = do
           rs <- decode tree
           case breakWord8 rs of
             (15, 0) -> do
               modified' <- appendBitToEach al rzz 15
               let modified = modified' ++ [0]
               helper (k + (fromIntegral $ length modified)) (drop (fromIntegral $ length modified) rzz) $ modified : lzz
             (r, 0) -> do
               o <- receive r
               (cnt, b, pred, _) <- get
               put (cnt, b, pred, 2 ^ r + o - 1)
               modified <- appendBitToEach al rzz (-1)
               helper (se + 1) [] $  modified : lzz
             (r, s) -> do
               o' <- receive s
               modified <- appendBitToEach al rzz $ fromIntegral r
               let ml = fromIntegral $ length modified + 1
               helper (k + fromIntegral ml) (drop ml rzz) $ [(extend o' $ fromIntegral s) * (2 ^ al)] : modified : lzz

appendBitToEach :: (Bits a, Ord a) => Word8 -> [a] -> Int -> StateT BitState Parser [a]
appendBitToEach _ [] _ = return []
appendBitToEach _ (0 : vs) 0 = return []
appendBitToEach bitposition (0 : vs) countzeros = do
  rest <- appendBitToEach bitposition vs $ countzeros - 1
  return $ 0 : rest
appendBitToEach bitposition (v : vs) countzeros = do
  b <- nextBit
  rest <- appendBitToEach bitposition vs countzeros
  let d = (fromIntegral b) * (2 ^ (fromIntegral bitposition))
  return $ (if v < 0 then v - d else v + d) : rest

----------------------------------------------------------------------------------------------------------------------

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
