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

outputPPM :: FilePath -> V.Vector (V.Vector (U.Vector Int)) -> IO ()
outputPPM filename v = withFile filename WriteMode (\ handle -> do
  hPutStr handle "P6\n"
  hPutStr handle $ (show width) ++ " " ++ (show height) ++ " 255\n"
  mapM_ (f handle) [0 .. height - 1]
  )
  where f :: Handle -> Int -> IO ()
        f handle y = mapM_ g [0 .. width - 1]
          where g :: Int -> IO ()
                g x = BS.hPut handle $ BS.pack [ fromIntegral $ v V.! 0 V.! y U.! x
                                               , fromIntegral $ v V.! 1 V.! y U.! x
                                               , fromIntegral $ v V.! 2 V.! y U.! x
                                               ]
        width = U.length $ V.head $ V.head v
        height = V.length $ V.head v

outputCorrectImage :: JPEGState -> M.Map Word8 (V.Vector (U.Vector Int)) -> String -> IO ()
outputCorrectImage s m filename
  | isJFIF s && (M.size $ frameComponents $ frameHeader s) == 3 = outputPPM filename $ convertJFIFImage s m
  | otherwise = putStrLn (show $ M.size $ frameComponents $ frameHeader s) >> (mapM_ (\ (k, v) -> outputPGM ("output_" ++ (show k) ++ ".pgm") v) $ M.toList m)

main = do
  args <- getArgs
  when (null args) $ fail "No filename supplied"
  bs <- BS.readFile $ head args
  case feed (parse decodeJPEG bs) BS.empty of
    Done bs s -> putStrLn (show $ map fst $ applicationData s) >> outputCorrectImage s (decodeJPEG' s) (args !! 1)
    _ -> putStrLn "Fail"
