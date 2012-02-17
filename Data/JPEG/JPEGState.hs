module Data.JPEG.JPEGState where

import Control.Applicative ((<|>), pure)
import Control.Monad (foldM, when)
import Data.Attoparsec
import Data.Attoparsec.Binary (anyWord16be)
import Data.Default (Default, def)
import Data.Maybe (fromJust)
import qualified Data.Map as M
import qualified Data.ByteString as BS
import qualified Data.List as L
import Data.Compression.Huffman
import Data.Word
import Prelude hiding (take)

import Data.JPEG.Markers
import Data.JPEG.Util

type QuantizationTables = M.Map Word8 [Word16]

type HuffmanTrees = (M.Map Word8 (HuffmanTree Word8), M.Map Word8 (HuffmanTree Word8))

data ArithmeticConditioningTable = ArithmeticConditioningTableAC Int Int
                                 | ArithmeticConditioningTableDC Int Int
  deriving (Show)

type ApplicationData = BS.ByteString

data JPEGState = JPEGState { frameHeader                  :: FrameHeader
                           , quantizationTables           :: QuantizationTables
                           , huffmanTrees                 :: HuffmanTrees
                           , arithmeticConditioningTables :: [ArithmeticConditioningTable]
                           , restartInterval              :: Int
                           , applicationData              :: [(Int, ApplicationData)]
                           }
  deriving (Show)

data FrameComponent = FrameComponent { h  :: Word8
                                     , v  :: Word8
                                     , tq :: Word8
                                     }
  deriving (Show)

type FrameComponents = M.Map Word8 FrameComponent

data FrameHeader = FrameHeader { n               :: Int
                               , p               :: Int
                               , y               :: Word16
                               , x               :: Word16
                               , frameComponents :: FrameComponents
                               }
  deriving (Show)

data ScanComponent = ScanComponent { cs :: Word8
                                   , td :: Word8
                                   , ta :: Word8
                                   }
  deriving (Show)

data ScanHeader = ScanHeader { scanComponents :: [ScanComponent]
                             , ss             :: Int
                             , se             :: Int
                             , ah             :: Int
                             , al             :: Int
                             }
  deriving (Show)

instance Default JPEGState where
  def = JPEGState { frameHeader                  = FrameHeader 0 0 0 0 M.empty
                  , quantizationTables           = M.empty
                  , huffmanTrees                 = (M.empty, M.empty)
                  , arithmeticConditioningTables = []
                  , restartInterval              = 0
                  , applicationData              = []
                  }

parseQuantizationTables :: Parser QuantizationTables
parseQuantizationTables = do
  parseDQT
  lq <- anyWord16be
  parseSegments $ fromIntegral $ lq - 2
  where parseSegments len
          | len == 0 = return M.empty
          | len < 0 = fail "Quantization table ended before it should have"
          | otherwise = do
            pqtq <- anyWord8
            let (pq, tq) = breakWord8 pqtq
            qs <- qks pq
            rest <- parseSegments $ len - 1 - (((fromIntegral pq) + 1) * 64)
            return $ M.insert tq qs rest
        qks 0 = count 64 anyWord8    >>= return . map fromIntegral
        qks 1 = count 64 anyWord16be

parseQuantizationTablesState :: JPEGState -> Parser JPEGState
parseQuantizationTablesState s = do
  t <- parseQuantizationTables
  return s {quantizationTables = M.union t $ quantizationTables s}

insertIntoHuffmanTree :: Int -> a -> HuffmanTree a -> Maybe (HuffmanTree a)
insertIntoHuffmanTree depth value Empty
  | depth == 0 = Just $ Leaf value
  | otherwise = case insertIntoHuffmanTree (depth - 1) value Empty of
    Nothing -> Nothing
    Just a -> Just $ Node a Empty
insertIntoHuffmanTree depth value (Node l r)
  | depth == 0 = Nothing
  | otherwise = case insertIntoHuffmanTree (depth - 1) value l of
    Nothing -> case insertIntoHuffmanTree (depth - 1) value r of
      Nothing -> Nothing
      Just a -> Just $ Node l a
    Just a -> Just $ Node a r
insertIntoHuffmanTree depth value (Leaf _) = Nothing

parseHuffmanTrees :: Parser HuffmanTrees
parseHuffmanTrees = do
  parseDHT
  lh <- anyWord16be
  parseSegments $ fromIntegral $ lh - 2
  where parseSegments len
          | len == 0 = return (M.empty, M.empty)
          | len < 0 = fail "Huffman table ended before it should have"
          | otherwise = do
            tcth <- anyWord8
            let (tc, th) = breakWord8 tcth
            lengths <- count 16 anyWord8
            vs <- mapM (\ l -> count (fromIntegral l) anyWord8) lengths
            let huffman_tree = fromJust $ foldM (\ a (d, v) -> insertIntoHuffmanTree d v a) Empty $ concat $ map (\ (v', d) -> map (\ v -> (d, v)) v') $ zip vs [1..]
            (dc, ac) <- parseSegments $ len - 1 - 16 - (fromIntegral $ sum $ map length vs)
            return $ if tc == 0
              then (M.insert (fromIntegral th) huffman_tree dc, ac)
              else (dc, M.insert (fromIntegral th) huffman_tree ac)

parseHuffmanTreesState :: JPEGState -> Parser JPEGState
parseHuffmanTreesState s = do
  (dc_m', ac_m') <- parseHuffmanTrees
  let (dc_m, ac_m) = huffmanTrees s
  return s {huffmanTrees = (M.union dc_m dc_m', M.union ac_m ac_m')}

parseArithmeticConditioningTables :: Parser [ArithmeticConditioningTable]
parseArithmeticConditioningTables = do
  parseDAC
  la <- anyWord16be
  parseSegments $ fromIntegral $ la - 2
  where parseSegments len
          | len == 0 = return []
          | len < 0 = fail "Arithmetic conditioning table ended before it should have"
          | otherwise = do
            tctb <- anyWord8
            let (tc, tb) = breakWord8 tctb
            cs <- anyWord8
            rest <- parseSegments $ len - 2
            return $ (if tc == 0
              then ArithmeticConditioningTableDC (fromIntegral tb) (fromIntegral cs)
              else ArithmeticConditioningTableAC (fromIntegral tb) (fromIntegral cs)) : rest

parseArithmeticConditioningTablesState :: JPEGState -> Parser JPEGState
parseArithmeticConditioningTablesState s = do
  t <- parseArithmeticConditioningTables
  return s {arithmeticConditioningTables = t}

parseRestartInterval :: Parser Int
parseRestartInterval = do
  parseDRI
  lr <- anyWord16be
  when (lr /= 4) $ fail "DRI malformed"
  ri <- anyWord16be
  return $ fromIntegral ri

parseRestartIntervalState :: JPEGState -> Parser JPEGState
parseRestartIntervalState s = do
  i <- parseRestartInterval
  return s {restartInterval = i}

parseComment :: Parser BS.ByteString
parseComment = do
  parseCOM
  lc <- anyWord16be
  take $ (fromIntegral lc) - 2

parseCommentState :: JPEGState -> Parser JPEGState
parseCommentState s = do
  _ <- parseComment
  return s

parseApplicationData :: Parser (Int, ApplicationData)
parseApplicationData = do
  n <- parseAPP
  lp <- anyWord16be
  d <- take $ (fromIntegral lp) - 2
  return $ (fromIntegral n, d)

parseApplicationDataState :: JPEGState -> Parser JPEGState
parseApplicationDataState s = do
  ad <- parseApplicationData
  return s {applicationData = ad : applicationData s}

parseTablesMisc :: JPEGState -> Parser JPEGState
parseTablesMisc s = parseThem <|> pure s
  where parseThem = parseOne >>= parseTablesMisc
        parseOne =   parseQuantizationTablesState s
                 <|> parseApplicationDataState s
                 <|> parseHuffmanTreesState s
                 <|> parseArithmeticConditioningTablesState s
                 <|> parseRestartIntervalState s
                 <|> parseCommentState s

parseScanHeader :: Parser ScanHeader
parseScanHeader = do
  parseSOS
  ls <- anyWord16be
  ns <- anyWord8
  component_specifications <- count (fromIntegral ns) parseComponentSpecification
  ss <- anyWord8
  se <- anyWord8
  ahal <- anyWord8
  let (ah, al) = breakWord8 ahal
  return $ ScanHeader component_specifications
                      (fromIntegral ss)
                      (fromIntegral se)
                      (fromIntegral ah)
                      (fromIntegral al)
  where parseComponentSpecification = do
          cs <- anyWord8
          tdta <- anyWord8
          let (td, ta) = breakWord8 tdta
          return $ ScanComponent cs td ta

parseFrameHeader :: Parser FrameHeader
parseFrameHeader = do
  n <- parseSOF
  lf <- anyWord16be
  p <- anyWord8
  y <- anyWord16be
  x <- anyWord16be
  nf <- anyWord8
  component_specifications <- (sequence $ L.replicate (fromIntegral nf) parseComponent) >>= return . M.fromList
  return $ FrameHeader (fromIntegral n)
                       (fromIntegral p)
                       (fromIntegral y)
                       (fromIntegral x)
                       component_specifications
  where parseComponent = do
          c <- anyWord8
          hv <- anyWord8
          let (h, v) = breakWord8 hv
          tq <- anyWord8
          return $ (fromIntegral c, FrameComponent (fromIntegral h)
                                                   (fromIntegral v)
                                                   (fromIntegral tq))
