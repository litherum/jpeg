module Data.JPEG.ProgressiveDCT where

import Control.Applicative
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

type DataUnitFunc = [Int] -> Word8 -> HuffmanTree Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> StateT BitState Parser [Int]

decodeDCDataUnit :: DataUnitFunc
decodeDCDataUnit _ c tree _ _ _ al = do
  dc <- diff c tree
  return $ (dc * (2 ^ al)) : replicate 63 0

decodeSubsequentDCScans :: DataUnitFunc
decodeSubsequentDCScans existing _ _ 0 0 ah al = do
  d <- receive (ah - al)
  when ((head existing) .&. ((2 ^ ah) - 1) /= 0) $ trace "These bits are already set!" $ lift $ fail "These bits are already set!"
  return $ ((head existing) + (d * (2 ^ al))) : (tail existing)

-- F.2.2.2
decodeACCoefficients :: DataUnitFunc
decodeACCoefficients existing _ tree ss se _ al = do
  mapM_ (\ x -> when (x /= 0) $ trace "These bits are already set!" $ lift $ fail "These bits are already set!") $ middle
  (cnt, b, pred, eobrun) <- get
  band <- if eobrun == 0
            then helper (fromIntegral $ se - ss + 1) []
            else do
              put (cnt, b, pred, eobrun - 1)
              return $ replicate (fromIntegral $ se - ss + 1) 0
  return $ beginning ++ band ++ end
  where beginning = L.take (fromIntegral ss) existing
        middle = L.take (fromIntegral $ se - ss + 1) $ L.drop (fromIntegral ss) existing
        end = L.drop (fromIntegral $ se + 1) existing
        helper k zz
         | k < 0 = trace "AC elements not properly aligned" $ lift $ fail "AC elements not properly aligned"
         | k == 0 = return $ concat $ reverse zz
         | otherwise = do
           rs <- decode tree
           let (r, s) = breakWord8 rs
           if s == 0
             then if r == 15
               then helper (k - 16) $ (replicate 16 0) : zz
               else do
                 o <- receive r
                 let eobrun = 2 ^ r + o
                 (cnt, b, pred, _) <- get
                 put (cnt, b, pred, eobrun - 1)
                 helper 0 $ (replicate k 0) : zz
             else do
               o' <- receive s
               let o = extend o' $ fromIntegral s
               helper (k - (fromIntegral r) - 1) $ [o * (2 ^ al)] : (replicate (fromIntegral r) 0) : zz

decodeSubsequentACScans :: DataUnitFunc
decodeSubsequentACScans existing _ tree ss se ah al = do
  mapM_ (\ x -> when (x .&. ((2 ^ ah) - 1) /= 0) $ trace "These bits are already set!" $ lift $ fail "These bits are already set!") middle
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
           let (r, s)  = breakWord8 rs
           if s == 0
             then if r == 15
               then do
                 modified' <- appendBitToEach al rzz 15
                 let modified = modified' ++ [0]
                 helper (k + (fromIntegral $ length modified)) (drop (fromIntegral $ length modified) rzz) $ modified : lzz
               else do
                 o <- receive r
                 let run_length = 2 ^ r + o
                 (cnt, b, pred, _) <- get
                 put (cnt, b, pred, run_length - 1)
                 modified <- appendBitToEach al rzz (-1)
                 helper (se + 1) [] $  modified : lzz
             else do
               when (s /= 1) $ trace "s != 1" $ lift $ fail "s != 1"
               o' <- receive s
               let o = extend o' $ fromIntegral s
               when (o /= 1 && o /= -1) $ trace "o is not +- 1" $ lift $ fail "o is not +- 1"
               modified <- appendBitToEach al rzz $ fromIntegral r
               let ml = fromIntegral $ length modified + 1
               helper (k + fromIntegral ml) (drop ml rzz) $ [o * (2 ^ al)] : modified : lzz

appendBitToEach :: Bits a => Word8 -> [a] -> Int -> StateT BitState Parser [a]
appendBitToEach _ [] _ = return []
appendBitToEach _ (0 : vs) 0 = return []
appendBitToEach bitposition (0 : vs) countzeros = do
  rest <- appendBitToEach bitposition vs $ countzeros - 1
  return $ 0 : rest
appendBitToEach bitposition (v : vs) countzeros = do
  b <- nextBit
  rest <- appendBitToEach bitposition vs countzeros
  return $ (v + (fromIntegral b) * (2 ^ (fromIntegral bitposition))) : rest

----------------------------------------------------------------------------------------------------------------------

decodeMCU :: [(Int, Word8, HuffmanTree Word8)] -> Word8 -> Word8 -> Word8 -> Word8 -> [[Int]] -> DataUnitFunc -> StateT BitState Parser [[[Int]]]
decodeMCU info ss se ah al existing dataUnitFunc = helper info existing []
  where helper [] _ l = return $ reverse l
        helper ((count, component, tree) : t) es l = do
          data_unit <- mapM (\ e -> dataUnitFunc e component tree ss se ah al) $ L.take (fromIntegral count) es
          helper t (L.drop (fromIntegral count) es) $ data_unit : l

decodeRestartIntervals :: [(Int, Word8, HuffmanTree Word8)] -> Word8 -> Word8 -> Word8 -> Word8 -> Word16 -> [[[Int]]] -> DataUnitFunc -> Parser [[[[Int]]]]
decodeRestartIntervals info ss se ah al ri existing dataUnitFunc = do
  o <- helper 0 (0, 0, M.fromList $ map (\ (_, c, _) -> (c, 0)) info, 0) existing []
  return $ reverse o
  where helper c s [] l = return l
        helper c s (e : es) l = do
          s' <- if ri /= 0 && c /= 0 && c `mod` ri == 0
            then do  -- Restart interval
              ri' <- parseRST
              when (ri' /= (fromIntegral $ ((c `quot` ri) - 1) `mod` 8)) $ trace "Restart interval incorrect" $ fail "Restart interval incorrect"
              return (0, 0, M.fromList $ map (\ (_, c, _) -> (c, 0)) info, 0)
            else return s
          (mcu, s'') <- runStateT (decodeMCU info ss se ah al e dataUnitFunc) s'
          (helper (c + 1) s'' es $ mcu : l) <|> (trace "Failed" $ (return $ mcu : l))

parseScan :: FrameHeader -> StateT JPEGState Parser ()
parseScan frame_header = do
  s' <- get
  s <- lift $ parseTablesMisc s'
  scan_header <- lift $ parseScanHeader
  trace (show scan_header) $ return ()
  case scan_header of
    ScanHeader scan_components 0 0 0 al -> do -- first DC Scan
      trace "First DC Scan!" $ return ()
      updated <- lift $ decodeRestartIntervals
        (map (component2Info s) scan_components)
        0 0 0 al (restartInterval s)
        (repeat (repeat []))
        decodeDCDataUnit
      put s {partialData = foldl1 M.union $ zipWith apply scan_components $ componentize updated}
    ScanHeader scan_components 0 0 ah al -> do -- successive DC Scan
      trace "Successive DC Scan!" $ return ()
      updated <- lift $ decodeRestartIntervals
        (map (component2Info s) scan_components)
        0 0 ah al (restartInterval s)
        (interleaveComponents $ map (breakUp (partialData s)) scan_components)
        decodeSubsequentDCScans
      put s {partialData = foldl1 M.union $ zipWith apply scan_components $ componentize updated}
    ScanHeader [scan_component] ss se 0 al -> do -- first AC Scan
      trace "First AC Scan!" $ return ()
      updated <- lift $ decodeRestartIntervals
        [( 1
         , cs scan_component
         , (snd $ huffmanTrees s) M.! (ta scan_component)
         )]
        ss se 0 al (restartInterval s)
        (map return $ (partialData s) M.! (cs scan_component))
        decodeACCoefficients
      put s {partialData = M.insert (cs scan_component) (map (head . head) updated) $ partialData s}
    ScanHeader [scan_component] ss se ah al -> do -- successive AC Scan
      trace "Successive AC Scan!" $ return ()
      updated <- lift $ decodeRestartIntervals
        [( 1
         , cs scan_component
         , (snd $ huffmanTrees s) M.! (ta scan_component)
         )]
        ss se ah al (restartInterval s)
        (map return $ (partialData s) M.! (cs scan_component))
        decodeSubsequentACScans
      put s {partialData = M.insert (cs scan_component) (map (head . head) updated) $ partialData s}
  where component2Info s (ScanComponent cs td ta) = (count, cs, tree)
          where frame_component = (frameComponents frame_header) M.! cs
                count = fromIntegral $ h frame_component * v frame_component
                tree = (fst $ huffmanTrees s) M.! td
        apply (ScanComponent cs _ _) updated = M.singleton cs $ blockOrder width_in_clusters cluster_width cluster_height updated
          where width_in_clusters = width_in_blocks `roundUp` (cluster_width * 8)
                cluster_width = fromIntegral $ h $ (frameComponents frame_header) M.! cs
                cluster_height = fromIntegral $ v $ (frameComponents frame_header) M.! cs
                width_in_blocks = ((fromIntegral $ x frame_header) * cluster_width) `roundUp` max_x
                max_x = fromIntegral $ foldl1 max $ map h $ M.elems $ frameComponents frame_header
        breakUp :: M.Map Word8 [[Int]] -> ScanComponent -> [[[Int]]]
        breakUp partial_data (ScanComponent cs _ _) = reverseBlockOrder width_in_blocks cluster_width cluster_height component_data
          where component_data = partial_data M.! cs
                cluster_width = fromIntegral $ h $ (frameComponents frame_header) M.! cs
                cluster_height = fromIntegral $ v $ (frameComponents frame_header) M.! cs
                width_in_blocks = ((fromIntegral $ x frame_header) * cluster_width) `roundUp` max_x
                max_x = fromIntegral $ foldl1 max $ map h $ M.elems $ frameComponents frame_header
        interleaveComponents ([] : _) = []
        interleaveComponents l = (concat $ map head l) : (interleaveComponents $ map tail l)

decodeFrame :: Parser (JPEGState, M.Map Word8 [[Int]])
decodeFrame = do
  s <- parseTablesMisc def
  frame_header <- parseFrameHeader
  when (n frame_header /= 2) $ fail "Unsupported frame!"
  s' <- execStateT (parseScan frame_header) s
  y' <- parseDNLSegment <|> (return $ y frame_header)
  let frame_header' = frame_header {y = y'}
  s'' <- parseScans frame_header' s'
  return $ (s'', decodeJPEG' frame_header' s'' $ partialData s'')
  where parseScans frame_header s = do
          s' <- execStateT (parseScan frame_header) s
          (parseScans frame_header s') <|> (return s')

decodeJPEG :: Parser (JPEGState, M.Map Word8 [[Int]])
decodeJPEG = do
  parseSOI
  o <- decodeFrame
  parseEOI
  trace "HOLEY MOLEY" $ return o
