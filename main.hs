import Data.Attoparsec
import qualified Data.ByteString as BS
import Prelude hiding (readFile)
import qualified Data.Map as M
import System.IO
import Data.Word
import System.Environment (getArgs)
import Control.Monad (when)

import Data.JPEG.Parser
import Data.JPEG.JFIF
import Data.JPEG.JPEGState

outputPGM :: FilePath -> [[Word8]] -> IO ()
outputPGM filename v = withFile filename WriteMode (\ h -> do
  hPutStr h "P5\n"
  hPutStr h $ (show $ length $ head v) ++ " " ++ (show $ length v) ++ " 255\n"
  mapM_ (\ l -> BS.hPut h $ BS.pack $ map fromIntegral l) v)

outputPPM :: FilePath -> [[(Word8, Word8, Word8)]] -> IO ()
outputPPM filename v = withFile filename WriteMode (\ h -> do
  hPutStr h "P6\n"
  hPutStr h $ (show $ length $ head v) ++ " " ++ (show $ length v) ++ " 255\n"
  mapM_ (\ l -> mapM_ (\ (r, g, b) -> BS.hPut h (BS.pack [fromIntegral r, fromIntegral g, fromIntegral b])) l) v)
  

outputCorrectImage :: (JPEGState, M.Map Word8 [[Word8]]) -> IO ()
outputCorrectImage a@(s, m)
  | isJFIF s && (M.size $ frameComponents $ frameHeader s) == 3 = outputPPM "output.pgm" $ convertJFIFImage a
  | otherwise = mapM_ (\ (k, v) -> outputPGM ("output_" ++ (show k) ++ ".pgm") v) $ M.toList m

conv = M.map (map (map fromIntegral))

main = do
  args <- getArgs
  when (null args) $ fail "No filename supplied"
  bs <- BS.readFile $ head args
  case feed (parse decodeJPEG bs) BS.empty of
    Done bs (s, r) -> putStrLn "Success!" >> outputCorrectImage (s, conv r)
    _ -> putStrLn "Fail"
