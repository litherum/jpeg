module Data.JPEG.Util where

import Data.Bits
import Data.Word
import Control.Applicative
import Data.Attoparsec

breakWord8 :: Word8 -> (Word8, Word8)
breakWord8 w = (w `shiftR` 4, w .&. 0xF)

many0 :: Alternative f => f a -> f [a]
many0 x = many1 x <|> pure []
