module Data.JPEG.Instances where

import Control.DeepSeq
import Data.Compression.Huffman
import qualified Data.ByteString as BS
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

instance NFData a => NFData (HuffmanTree a) where
  rnf Empty = ()
  rnf (Node l r) = l `deepseq` r `deepseq` ()
  rnf (Leaf a) = a `deepseq` ()

instance NFData a => NFData (V.Vector a) where
  rnf v = V.foldl (const (`deepseq` ())) () v

instance NFData a => NFData (U.Vector a)

instance NFData BS.ByteString
