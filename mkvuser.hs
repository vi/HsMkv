module Main where

import qualified Data.ByteString.Lazy as B
import Data.Bits

import System.IO
import Text.Printf

data EbmlNumberType = Unsigned | Signed | Unmodified  deriving (Show, Eq, Ord)

getMajorBit :: (Bits a) => a -> Int
getMajorBit x
    | x == 0                 = error "getMajorBit 0?"
    | (x .&. 0x80) == 0x80   = 0
    | otherwise              = 1 + getMajorBit (shiftL x 1)

readEbmlNumber :: EbmlNumberType -> B.ByteString -> (Integer, B.ByteString)
readEbmlNumber Unmodified b = renStage2 additionalSize (toInteger head) tail  
    where
    head = B.head(b)
    tail = B.tail(b)
    additionalSize = getMajorBit head
    renStage2 :: Int -> Integer -> B.ByteString -> (Integer, B.ByteString)
    renStage2 0 x b = (x, b)
    renStage2 n x b = renStage2 (n-1) ((shiftL x 8) .|. (toInteger $ B.head b)) (B.tail b)
readEbmlNumber Unsigned b = (x2, tail)
    where
    additionalSize = getMajorBit $ B.head b
    (x, tail) = readEbmlNumber Unmodified b
    x2 = xor x $ shiftL (shiftR 0x80 $ additionalSize) (additionalSize * 8)
readEbmlNumber Signed b = (x2, tail)
    where
    additionalSize = getMajorBit $ B.head b
    (x, tail) = readEbmlNumber Unsigned b
    x2 = x - (2 ^ (6 + 7 * additionalSize) - 1)

parseMkv :: B.ByteString -> IO ()
parseMkv b = printf "%x\n" $ fst (readEbmlNumber Unmodified b)
-- parseMkv _ = return ()


main :: IO ()
main = do
    B.readFile "q.mkv" >>= parseMkv
    -- B.readFile "q.mkv" >>= \contents -> B.writeFile "qq.mkv" contents
    {- h <- openFile "q.mkv" ReadMode
    ho <- openFile "qq.mkv" WriteMode
    hSetBuffering h (BlockBuffering $ Just 1024)
    hSetBuffering ho (BlockBuffering $ Just 1024)
    content <- B.hGetContents h
    B.hPutStr ho content -}
