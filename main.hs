import Data.Attoparsec
import qualified Data.ByteString as BS
import Prelude hiding (readFile)
import qualified Data.Map as M
import System.IO
import Data.Word

import Data.JPEG.Control

outputPGM :: FilePath -> [[Word8]] -> IO ()
outputPGM filename v = withFile filename WriteMode (\ h -> do
  hPutStr h "P5\n"
  hPutStr h $ (show $ length $ head v) ++ " " ++ (show $ length v) ++ " 255\n"
  mapM_ (\ l -> BS.hPut h $ BS.pack $ map fromIntegral l) v)

main = do
  --bs <- BS.readFile "2878123.jpg"
  --bs <- BS.readFile "70024.jpg"
  bs <- BS.readFile "1.jpg"
  case feed (parse decodeJPEG bs) BS.empty of
    Done bs r -> putStrLn "Success!" >> (mapM_ (\ (k, v) -> outputPGM ("output_" ++ (show k) ++ ".pgm") v) $ M.toList r)
    _ -> putStrLn "Fail"
