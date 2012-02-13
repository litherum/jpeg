import System.IO
import Data.Attoparsec.ByteString
import Data.Attoparsec.Binary
import Data.Bits
import Data.Word
import Control.Applicative hiding (empty)
import Prelude hiding (take, readFile, take, take)
import qualified Data.List as L
import qualified Data.ByteString as BS
import Control.Monad (when)
import Data.Default
import Data.ByteString (empty)
import Data.Compression.Huffman
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Control.Monad (foldM)
import Control.Monad.State
import Debug.Trace (trace)
import qualified Data.Vector as V

type QuantizationTables = M.Map Int [Int]
type HuffmanTrees = (M.Map Int (HuffmanTree Int), M.Map Int (HuffmanTree Int))
data ArithmeticConditioningTable = ArithmeticConditioningTableAC Int Int
                                 | ArithmeticConditioningTableDC Int Int
  deriving (Show)
data ApplicationData = ApplicationData BS.ByteString
  deriving (Show)
data ScanComponent = ScanComponent Int Int Int
  deriving (Show)
data ScanHeader = ScanHeader [ScanComponent] Int Int Int Int
  deriving (Show)
data FrameComponent = FrameComponent Int Int Int
  deriving (Show)
type FrameComponents = M.Map Int FrameComponent
data FrameHeader = FrameHeaderBaselineDCTHuffman              Int Int Int FrameComponents
                 | FrameHeaderExtendedSequentialDCTHuffman    Int Int Int FrameComponents
                 | FrameHeaderProgressiveDCTHuffman           Int Int Int FrameComponents
                 | FrameHeaderLosslessSequentialHuffman       Int Int Int FrameComponents
                 | FrameHeaderExtendedSequentialDCTArithmetic Int Int Int FrameComponents
                 | FrameHeaderProgressiveDCTArithmetic        Int Int Int FrameComponents
                 | FrameHeaderLosslessSequentialArithmetic    Int Int Int FrameComponents
  deriving (Show)
data JPEGState = JPEGState { quantizationTables           :: QuantizationTables
                           , huffmanTrees                 :: HuffmanTrees
                           , arithmeticConditioningTables :: [ArithmeticConditioningTable]
                           , restartInterval              :: Int
                           , applicationData              :: [(Int, ApplicationData)]
                           }
  deriving (Show)

instance Default JPEGState where
  def = JPEGState { quantizationTables           = M.empty
                  , huffmanTrees                 = (M.empty, M.empty)
                  , arithmeticConditioningTables = []
                  , restartInterval              = 0
                  , applicationData              = []
                  }

type Image = M.Map Int [[Int]]

breakWord8 :: Word8 -> (Word8, Word8)
breakWord8 w = (w `shiftR` 4, w .&. 0xF)

many0 :: Alternative f => f a -> f [a]
many0 x = many1 x <|> pure []

parseSOI :: Parser ()
parseSOI = do
  word8 0xFF
  word8 0xD8
  return ()

parseEOI :: Parser ()
parseEOI = do
  word8 0xFF
  word8 0xD9
  return ()

parseDQT :: Parser ()
parseDQT = do
  word8 0xFF
  word8 0xDB
  return ()

parseDHT :: Parser ()
parseDHT = do
  word8 0xFF
  word8 0xC4
  return ()

parseDAC :: Parser ()
parseDAC = do
  word8 0xFF
  word8 0xCC
  return ()

parseDRI :: Parser ()
parseDRI = do
  word8 0xFF
  word8 0xDD
  return ()

parseCOM :: Parser ()
parseCOM = do
  a <- word8 0xFF
  b <- word8 0xFE
  return ()

parseAPP :: Parser Int
parseAPP = do
  word8 0xFF
  w <- anyWord8
  if w >= 0xE0 && w <= 0xEF
    then return $ fromIntegral $ w - 0xE0
    else fail "APPn marker not recognized"

parseDNL :: Parser ()
parseDNL = do
  word8 0xFF
  word8 0xDC
  return ()

parseSOS :: Parser ()
parseSOS = do
  word8 0xFF
  word8 0xDA
  return ()

parseSOF :: Parser Int
parseSOF = do
  word8 0xFF
  w <- anyWord8
  if w >= 0xC0 && w <= 0xCF && w /= 0xC4 && w /= 0xCC
    then return $ fromIntegral $ w - 0xC0
    else fail "SOFn marker not recognized"

parseRST :: Parser Int
parseRST = do
  word8 0xFF
  w <- anyWord8
  if w >= 0xD0 && w <= 0xD7
    then return $ fromIntegral $ w - 0xD0
    else fail "RSTn marker incorrect"

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
            return $ M.insert (fromIntegral tq) qs rest
        qks 0 = count 64 anyWord8    >>= return . map fromIntegral
        qks 1 = count 64 anyWord16be >>= return . map fromIntegral

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
            vs <- mapM (\ l -> count (fromIntegral l) anyWord8 >>= return . map fromIntegral) lengths
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
  return $ (fromIntegral n, ApplicationData d)

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

parseNumberOfLines :: Parser Int
parseNumberOfLines = do
  parseDNL
  ld <- anyWord16be
  when (ld /= 4) $ fail "DNL malformed"
  nl <- anyWord16be
  return $ fromIntegral nl

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
          return $ ScanComponent (fromIntegral cs)
                                 (fromIntegral td)
                                 (fromIntegral ta)

parseFrameHeader :: Parser FrameHeader
parseFrameHeader = do
  n <- parseSOF
  lf <- anyWord16be
  p <- anyWord8
  y <- anyWord16be
  x <- anyWord16be
  nf <- anyWord8
  component_specifications <- (sequence $ L.replicate (fromIntegral nf) parseComponent) >>= return . M.fromList
  return $ (case n of
    0 ->  FrameHeaderBaselineDCTHuffman
    1 ->  FrameHeaderExtendedSequentialDCTHuffman
    2 ->  FrameHeaderProgressiveDCTHuffman
    3 ->  FrameHeaderLosslessSequentialHuffman
    9 ->  FrameHeaderExtendedSequentialDCTArithmetic
    10 -> FrameHeaderProgressiveDCTArithmetic
    11 -> FrameHeaderLosslessSequentialArithmetic
    ) (fromIntegral p)
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

type BitState = (Word8, Word8) -- (Index, Data)

getNextByte :: StateT BitState Parser ()
getNextByte = do
  (_, d) <- get
  d' <- lift anyWord8
  case (d, d') of
    (0xFF, 0x00) -> do
      d'' <- lift anyWord8
      put (0, d'')
    (0xFF, _) -> fail "Hit a marker during huffman decode process"
    _ -> put (0, d')

getBits :: Int -> StateT BitState Parser [Bool]
getBits c = do
  (i, d) <- get
  if c <= (fromIntegral $ 8 - i)
    then do
      put (i + (fromIntegral c), d)
      return $ L.take c $ L.drop (fromIntegral i) $ toBits d
    else do
      getNextByte
      l <- getBits (c - (fromIntegral $ 8 - i))
      return $ (drop (fromIntegral i) $ toBits d) ++ l
  where toBits d = map (testBit d) [7,6..0]

parseNextHuffVal :: Show a => HuffmanTree a -> StateT BitState Parser a
parseNextHuffVal Empty = fail "Value is not in huffman tree"
parseNextHuffVal (Leaf a) = return a
parseNextHuffVal (Node l r) = do
  [c] <- getBits 1
  parseNextHuffVal $ if c then r else l

decodeInt :: Int -> StateT BitState Parser Int
decodeInt v
  | v == 0 = return 0
  | otherwise = do
      (l : ls) <- getBits v
      return $ (if l then 2^(v-1) else ((-(2^v))+1)) + (fromBits ls)
  where fromBits = foldl (\ a b -> a `shiftL` 1 + (if b then 1 else 0)) 0

parseDC :: Int -> HuffmanTree Int -> StateT BitState Parser Int
parseDC pred dc_tree = do
  v <- parseNextHuffVal dc_tree >>= decodeInt
  return $ v + pred

parseAC :: Int -> HuffmanTree Int -> StateT BitState Parser [Int]
parseAC c ac_tree
 | c == 0 = return []
 | c < 0 = fail "AC elements not properly aligned"
 | otherwise = do
   v <- parseNextHuffVal ac_tree
   let (r, s) = breakWord8 $ fromIntegral v
   case (r, s) of
     (0x00, 0x00) -> return $ L.take c $ L.repeat 0
     (0x0F, 0x00) -> do
       acs <- parseAC (c - 16) ac_tree
       return $ (L.take 16 $ L.repeat 0) ++ acs
     _ -> do
       when (s == 0) $ fail "s is 0!"
       s' <- decodeInt $ fromIntegral s
       acs <- parseAC (c - (fromIntegral r) - 1) ac_tree
       return $ (L.take (fromIntegral r) $ L.repeat 0) ++ [fromIntegral s'] ++ acs

gatherByComponent :: [[a]] -> [[a]]
gatherByComponent x
  | null $ head x = []
  | otherwise = (map head x) : (gatherByComponent (map tail x))

parseScan :: JPEGState -> Int -> Int -> Parser (JPEGState, Int, Int, M.Map Int [[Int]])
parseScan s x y = do
  s' <- parseTablesMisc s
  let (dc_m, ac_m) = huffmanTrees s'
  ScanHeader components ss se ah al <- parseScanHeader
  iv <- anyWord8
  o <- evalStateT (parseECS components dc_m ac_m ss se ((roundUp x 8) * (roundUp y 8)) (restartInterval s')) (0, iv)
  let m = M.fromList $ zip (map (\ (ScanComponent cs _ _) -> cs) components) $ gatherByComponent o
  return (s', ss, se, m)

parseECS :: [ScanComponent] -> M.Map Int (HuffmanTree Int) -> M.Map Int (HuffmanTree Int) -> Int -> Int -> Int -> Int -> StateT BitState Parser [[[Int]]]
parseECS components dc_m ac_m ss se n restart_interval = evalStateT (parseECSHelper n) (0, 0)
  where parseECSHelper :: Int -> StateT (Int, Int) (StateT BitState Parser) [[[Int]]]
        parseECSHelper c
          | restart_interval == 0 || c <= restart_interval = parseRun c
          | otherwise = do
            o <- parseRun restart_interval
            (pred, rst) <- get
            rn <- lift $ lift parseRST
            let rst' = if rst == 7 then 0 else rst + 1
            when (rn /= rst') $ fail "Incorrect RST"
            put (pred, rst')
            r <- parseECSHelper (c - restart_interval)
            return $ o ++ r
        parseRun x = sequence $ L.take x $ L.repeat $ parseMacroblock components
        parseMacroblock components = mapM (\ (ScanComponent cs td ta) ->
          (if ss == 0 then parseComponentDC else parseComponentAC) (se - ss + 1) td ta) components
        parseComponentAC :: Int -> Int -> Int -> StateT (Int, Int) (StateT BitState Parser) [Int]
        parseComponentAC l td ta = lift $ parseAC l (fromJust $ M.lookup ta ac_m)
        parseComponentDC :: Int -> Int -> Int -> StateT (Int, Int) (StateT BitState Parser) [Int]
        parseComponentDC l td ta = do
          (pred, rst) <- get
          pred' <- lift $ parseDC pred (fromJust $ M.lookup td dc_m)
          put (pred', rst)
          acs <- parseComponentAC (l-1) td ta
          return $ pred' : acs

roundUp :: Integral a => a -> a -> a
roundUp t b
  | t `mod` b == 0 = t `div` b
  | otherwise = (t `div` b) + 1

applyScan :: Image -> Int -> Int -> M.Map Int [[Int]] -> Image
applyScan image ss se scan_result = M.foldWithKey f image scan_result
  where f k scan_component image' = case M.lookup k image' of
                                      Just ic -> M.insert k (zipWith f' ic scan_component) image'
                                      Nothing -> undefined
        f' c_mac partial = (L.take ss c_mac) ++ partial ++ (L.drop (ss + se + 1) c_mac)

parseFrame :: Parser Image
parseFrame = do
  parseSOI
  s <- parseTablesMisc def
  fh <- parseFrameHeader
  let (p, x, y, components) = case fh of
                                FrameHeaderBaselineDCTHuffman              p y x fc -> (p, x, y, fc)
                                FrameHeaderExtendedSequentialDCTHuffman    p y x fc -> (p, x, y, fc)
                                FrameHeaderProgressiveDCTHuffman           p y x fc -> (p, x, y, fc)
                                FrameHeaderLosslessSequentialHuffman       p y x fc -> (p, x, y, fc)
                                FrameHeaderExtendedSequentialDCTArithmetic p y x fc -> (p, x, y, fc)
                                FrameHeaderProgressiveDCTArithmetic        p y x fc -> (p, x, y, fc)
                                FrameHeaderLosslessSequentialArithmetic    p y x fc -> (p, x, y, fc)
  let image = M.map (\ _ -> replicate ((roundUp x 8) * (roundUp y 8)) $ replicate 64 0) components
  (s', ss, se, scan) <- parseScan s x y
  let image' = applyScan image ss se scan
  -- DNL segment goes here
  (_, image'') <- parseScanHelper s' x y image'
  let image''' = M.mapWithKey (\ k a -> case M.lookup k components of
                                Just (FrameComponent _ _ tq) -> map (conv . idct . zipWith (*) (fromJust $ M.lookup tq $ quantizationTables s')) a
                                Nothing -> fail "Image component does not correspond to a declared header component"
                              ) image'' 
  return $ M.map (reorganize x y) image'''
  where conv = (map $ ((clamp 0 255) . floor . (+ 128)))
        clamp l h a
          | a < l = l
          | a > h = h
          | otherwise = a

parseScanHelper :: JPEGState -> Int -> Int -> Image -> Parser (JPEGState, Image)
parseScanHelper s x y image = f (s, image)
  where f a = eoi a <|> scan a
        eoi a = parseEOI >> return a
        scan (s, image) = do
          (s', ss, se, scan) <- parseScan s x y
          f (s', applyScan image ss se scan)

idct :: [Int] -> [Double]
idct l = map f indices
  where f (x, y) = 0.25 * (sum $ map (\ ((u, v), a) -> g x y u v a) $ zip indices l)
        g x y u v a = (c u) * (c v) * (fromIntegral a) *
                      (cos (((2 * (fromIntegral x) + 1) * (fromIntegral u) * pi)/16)) *
                      (cos (((2 * (fromIntegral y) + 1) * (fromIntegral v) * pi)/16))
        c 0 = 1.0 / (sqrt 2.0)
        c _ = 1.0

indices :: [(Int, Int)]
indices = (concat $ map (\ x -> map (\ y -> (y, x-y)) (if x `mod` 2 == 0 then [0..x] else [x,x-1..0])) [0..7]) ++
          (concat $ map (\ x -> map (\ y -> (x-y, y)) (if x `mod` 2 == 0 then [7,6..x-7] else [x-7..7])) [8..15])

indices' :: Int -> Int -> Int
indices' x y = (s V.! y) V.! x
  where s = V.fromList $ map (\ y -> V.fromList $ map (\ x -> fromJust $ L.lookup (x, y) s') [0..7]) [0..7]
        s' = zip indices [0..]

reorganize :: Int -> Int -> [[Int]] -> [[Int]]
reorganize x y ls = map (\ n -> map (\ m -> (vecs V.! (count m n)) V.! (index m n)) [0..x-1]) [0..y-1]
  where vecs = V.fromList $ map V.fromList ls
        count m n = (m `div` 8) + ((roundUp x 8) * (n `div` 8))
        index m n = indices' (m `mod` 8) (n `mod` 8)

main = do
  bs <- BS.readFile "test.jpeg"
  let Done _ img = feed (parse parseFrame bs) BS.empty
  let arr = fromJust $ M.lookup 1 img
  withFile "out.pbm" WriteMode (\ h -> do
    hPutStr h "P5\n"
    hPutStr h $ (show $ length $ head arr) ++ " " ++ (show $ length arr) ++ " 255\n"
    mapM_ (\ l -> BS.hPut h $ BS.pack $ map fromIntegral l) arr)
