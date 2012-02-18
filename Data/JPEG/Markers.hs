module Data.JPEG.Markers where

import Data.Attoparsec
import Data.Attoparsec.Binary
import Data.Word

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

parseDNLSegment :: Parser Word16
parseDNLSegment = do
  parseDNL
  word16be 4
  anyWord16be

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
