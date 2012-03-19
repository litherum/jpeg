import Data.Attoparsec
import qualified Data.ByteString as BS

import Data.JPEG.ProgressiveDCT

main = do
  f <- BS.readFile "testprogressive.jpeg"
  let (Done _ r) = parse decodeJPEG f in putStrLn $ show r