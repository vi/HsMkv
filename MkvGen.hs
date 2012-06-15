module MkvGen where

import MkvParse
import MkvTabular

import Data.Bits
import Data.Word
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Data.Binary.IEEE754


unsignedBigEndianNumber :: Int -> Integer -> [Word8]
unsignedBigEndianNumber 0 _ = []
unsignedBigEndianNumber 1 x = [fromInteger x]
unsignedBigEndianNumber n x = head : tail
        where
        n' = n-1
        head = fromInteger $ (x `shiftR` (8*n')) .&. 0xFF 
        tail = unsignedBigEndianNumber (n-1) (x .&. (complement (0xFF `shiftL` (8*n'))))

getEbmlNumberSize x
    | x < 2^7 - 1   =  1
    | otherwise     = 1 + getEbmlNumberSize (((x+1) `shiftR` 7 - 1))

writeEbmlNumber :: EbmlNumberType -> Integer -> [Word8]
writeEbmlNumber ENUnsigned (-1) =  [255]
writeEbmlNumber ENUnsigned x = head : unsignedBigEndianNumber n' rest
    where
    n = (getEbmlNumberSize x)
    n' = n-1
    first_byte = toInteger $ (x `shiftR` (8*n')) .&. 0xFF
    head = fromInteger $ first_byte .|. (0x80 `shiftR` (n-1))
    rest = x .&. (complement (0xFF `shiftL` (8*n')))
writeEbmlNumber ENUnmodified x = head : unsignedBigEndianNumber n' rest
    where
    n' = (rank x - 7) `div` 7 
    first_byte = toInteger $ (x `shiftR` (8*n')) .&. 0xFF
    head = fromInteger first_byte
    rest = x .&. (complement (0xFF `shiftL` (8*n')))
    rank 0 = 0 -- just in case
    rank 1 = 0
    rank x = 1 + rank (x `shiftR` 1)
    
    
matroskaElement_raw :: EbmlElementID -> Bool -> B.ByteString -> B.ByteString
matroskaElement_raw id_ nosize content_ = B.concat [id_b, size_b, content_]
    where
    id_b = B.pack $ writeEbmlNumber ENUnmodified id_
    size = case nosize of
        True -> (-1)
        False -> toInteger $ B.length content_
    size_b = B.pack $ writeEbmlNumber ENUnsigned $ size


toMatroskaDate :: Double -> Integer
toMatroskaDate x = floor ((x - 978300000.0) * 1000000000.0)
-- 2001-01-01T00:00:00,000000000

niceNumberSize :: Integer -> Int
niceNumberSize x
    | x > (-129) && x < 128   = 1
    | otherwise = 1 + niceNumberSize (x `div` 256)

bigEndianNumber :: Bool -> Integer -> [Word8]
bigEndianNumber signed x = unsignedBigEndianNumber niceSize x'
    where
    niceSize = niceNumberSize x
    x' = case signed of
        False -> x
        True -> case x < 0 of
            False -> x
            True -> x + (2^(8*niceSize))



-- Convert MatroskaElement to Bytestring
-- size is ignored, unless "-1" which means "infinite" (for Segment)
writeMatroskaElement :: MatroskaElement -> B.ByteString
writeMatroskaElement (MatroskaElement kl declsize content) = 
    matroskaElement_raw (lookupElementIdReverse kl) nosize (encodeContent content)
    where
    nosize = declsize == (-1)
    encodeContent (EC_Binary b) = b
    encodeContent (EC_Unsigned b) = B.pack $ bigEndianNumber False  b
    encodeContent (EC_Signed b) = B.pack $ bigEndianNumber True b
    encodeContent (EC_Date b ) = B.pack $ writeEbmlNumber ENUnsigned $ toMatroskaDate b
    encodeContent (EC_TextAscii b) = B.fromChunks [E.encodeUtf8 b]
    encodeContent (EC_TextUtf8 b) = B.fromChunks [E.encodeUtf8 b]
    encodeContent (EC_Float b) =  B.pack $ unsignedBigEndianNumber 8 $ toInteger $ Data.Binary.IEEE754.doubleToWord b
    encodeContent (EC_Master b) = B.concat $ map writeMatroskaElement b

    
    

ebmlHeader :: MatroskaElement
ebmlHeader = MatroskaElement EE_EBML 0 $ EC_Master [
     MatroskaElement EE_EBMLVersion        0 $ EC_Unsigned 1
    ,MatroskaElement EE_EBMLReadVersion    0 $ EC_Unsigned 1
    ,MatroskaElement EE_EBMLMaxIDLength    0 $ EC_Unsigned 4
    ,MatroskaElement EE_EBMLMaxSizeLength  0 $ EC_Unsigned 8
    ,MatroskaElement EE_DocType            0 $ EC_TextAscii $ T.pack "matroska"
    ,MatroskaElement EE_DocTypeVersion     0 $ EC_Unsigned 2
    ,MatroskaElement EE_DocTypeReadVersion 0 $ EC_Unsigned 2
    ]

matroskaHeader :: B.ByteString
matroskaHeader = B.concat [
    writeMatroskaElement ebmlHeader,
    writeMatroskaElement $ MatroskaElement EE_Segment (-1) $ EC_Binary B.empty
    ]


rawFrame :: Integer -> Integer -> [B.ByteString] -> B.ByteString
rawFrame rel_timecode track buffers = B.concat [
     B.pack $ writeEbmlNumber ENUnsigned track
    ,B.pack $ unsignedBigEndianNumber 2 rel_timecode'
    ,B.pack [flags]
    ,B.pack lacing
    ,B.concat $ buffers]
    where
    rel_timecode' = case rel_timecode < 0 of
        False -> rel_timecode
        True -> rel_timecode + 0x8000
    flags = 0x02 -- Xiph lacing
    n = fromIntegral (length buffers ) :: Word8
    (buffersExpectOfLast, _) = splitAt (length buffers - 1) buffers
    lengths = map B.length buffersExpectOfLast
    xiph :: Int -> [Word8]
    xiph x
        | x < 255   = [fromIntegral x]
        | otherwise = 255:(xiph (x-255))
    lacing = (n-1) : (concat $ map (xiph . fromIntegral) lengths)


frameCluster :: Integer -> Frame -> MatroskaElement
frameCluster timecode_scale frame =
     MatroskaElement EE_Cluster 0 $ EC_Master [
        MatroskaElement EE_Timecode 0 $ EC_Unsigned $ toMatroskaTimecode (f_timeCode frame)
       ,case f_duration frame of
            Nothing -> MatroskaElement EE_SimpleBlock 0 $ EC_Binary frameData
            Just duration -> MatroskaElement EE_BlockGroup 0 $ EC_Master [
                 MatroskaElement EE_BlockDuration 0 $ EC_Unsigned $ toMatroskaTimecode duration
                ,MatroskaElement EE_Block 0 $ EC_Binary frameData 
                ]
    ] where
    toMatroskaTimecode :: Double -> Integer
    toMatroskaTimecode timecode = floor $ (timecode * 1000000000.0) / (fromInteger timecode_scale)
    frameData = rawFrame 0 (f_trackNumber frame) (f_data frame)
         
    
-- B.writeFile "g.mkv" $ B.concat [matroskaHeader, writeMatroskaElement $ frameCluster 1000000 $ Frame 1 45.4 [B.empty] (Just 2.5)]