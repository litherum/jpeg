import Data.Attoparsec
import qualified Data.ByteString as BS
import Prelude hiding (readFile)
import qualified Data.Map as M
import System.IO
import Data.Word
import System.Environment (getArgs)
import Control.Monad (when)

import Data.JPEG.ProgressiveDCT
import Data.JPEG.JPEGState

outputPGM :: FilePath -> [[Int]] -> IO ()
outputPGM filename v = withFile filename WriteMode (\ h -> do
  hPutStr h "P5\n"
  hPutStr h $ (show $ length $ head v) ++ " " ++ (show $ length v) ++ " 255\n"
  mapM_ (\ l -> BS.hPut h $ BS.pack $ map fromIntegral l) v)

outputCorrectImage :: (JPEGState, M.Map Word8 [[Int]]) -> IO ()
outputCorrectImage a@(s, m) = mapM_ (\ (k, v) -> outputPGM ("output_" ++ (show k) ++ ".pgm") v) $ M.toList m

main = do
  args <- getArgs
  when (null args) $ fail "No filename supplied"
  bs <- BS.readFile $ head args
  case feed (parse decodeJPEG bs) BS.empty of
    Done bs r -> putStrLn "Success!" >> outputCorrectImage r
    _ -> putStrLn "Fail"