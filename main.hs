import Data.Attoparsec
import qualified Data.ByteString as BS
import Prelude hiding (readFile)
import qualified Data.Map as M
import System.IO
import Data.Word
import System.Environment (getArgs)
import Control.Monad (when)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V

import Data.JPEG.Decode
import Data.JPEG.Parser
import Data.JPEG.JFIF
import Data.JPEG.JPEGState

outputPGM :: FilePath -> V.Vector (U.Vector Int) -> IO ()
outputPGM filename v = withFile filename WriteMode (\ h -> do
  hPutStr h "P5\n"
  hPutStr h $ (show $ U.length $ V.head v) ++ " " ++ (show $ V.length v) ++ " 255\n"
  V.mapM_ (U.mapM_ (\ i -> BS.hPut h $ BS.singleton $ fromIntegral i)) v)

{-
outputPPM :: FilePath -> [[(Word8, Word8, Word8)]] -> IO ()
outputPPM filename v = withFile filename WriteMode (\ h -> do
  hPutStr h "P6\n"
  hPutStr h $ (show $ length $ head v) ++ " " ++ (show $ length v) ++ " 255\n"
  mapM_ (\ l -> mapM_ (\ (r, g, b) -> BS.hPut h (BS.pack [fromIntegral r, fromIntegral g, fromIntegral b])) l) v)
-}

outputCorrectImage :: JPEGState -> M.Map Word8 (V.Vector (U.Vector Int)) -> String -> IO ()
outputCorrectImage s m filename
--  | isJFIF s && (M.size $ frameComponents $ frameHeader s) == 3 = outputPPM filename $ convertJFIFImage s m
  | otherwise = putStrLn (show $ M.size $ frameComponents $ frameHeader s) >> (mapM_ (\ (k, v) -> outputPGM ("output_" ++ (show k) ++ ".pgm") v) $ M.toList m)

--conv = M.map (V.map (U.map fromIntegral))

main = do
  args <- getArgs
  when (null args) $ fail "No filename supplied"
  bs <- BS.readFile $ head args
  case feed (parse decodeJPEG bs) BS.empty of
    Done bs s -> putStrLn (show $ map fst $ applicationData s) >> outputCorrectImage s (decodeJPEG' s) (args !! 1)
    _ -> putStrLn "Fail"
