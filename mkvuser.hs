module Main where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8
import qualified Data.ByteString.Lazy.UTF8
import Data.Bits
import Data.List -- foldl1'

import System.IO
import Text.Printf

type EbmlElementID = Integer
type EbmlElementName = String

data EbmlNumberType = ENUnsigned | ENSigned | ENUnmodified  deriving (Show, Eq, Ord)
data EbmlElementRaw = EbmlElementRaw EbmlElementID B.ByteString deriving (Show)

data EbmlElementType =
        ETMaster |
        ETUnsigned |
        ETSigned |
        ETTextAscii |
        ETTextUtf8 |
        ETBinary |
        ETFloat
        deriving (Show, Eq, Ord)

data EbmlElementData = 
        EN Integer |
        EB B.ByteString |
        ET String |
        EF Float |
        EM [EbmlElement]  -- can be infinite
    deriving (Show)

data EbmlElement = EbmlElement {
    eeId :: EbmlElementID,
    eeType :: EbmlElementType,
    eeName :: EbmlElementName,
    eeData :: EbmlElementData
    } deriving (Show)


getElementTypeAndName :: EbmlElementID -> (EbmlElementType, EbmlElementName)
getElementTypeAndName id_  = g id_
    where
    g 0x1A45DFA3 = (ETMaster, "EBML")
    g 0x4286 = (ETUnsigned, "EBMLVersion")
    g 0x42F7 = (ETUnsigned, "EBMLReadVersion")
    g 0x42F2 = (ETUnsigned, "EBMLMaxIDLength")
    g 0x42F3 = (ETUnsigned, "EBMLMaxSizeLength")
    g 0x4282 = (ETTextAscii, "DocType")
    g 0x4287 = (ETUnsigned, "DocTypeVersion")
    g 0x4285 = (ETUnsigned, "DocTypeReadVersion")
    g 0x18538067 = (ETMaster, "Segment")
    g 0x1549A966 = (ETMaster, "SegmentInfo")
    g 0x73A4 = (ETBinary, "SegmentUID")
    g 0x7BA9 = (ETTextUtf8, "Title")
    g 0x4D80 = (ETTextAscii, "MuxingApp")
    g 0x5741 = (ETTextAscii, "WritingApp")
    g 0x2A7DB1 = (ETUnsigned, "TimecodeScale")
    g 0x1654AE6B = (ETMaster, "Tracks")
    g 0xAE = (ETMaster, "TrackEntry")
    g 0xD7 = (ETUnsigned, "TrackNumber")
    g 0x73C5 = (ETUnsigned, "TrackUID")
    g 0x83 = (ETUnsigned, "TrackType")
    g 0xB9 = (ETUnsigned, "FlagEnabled")
    g 0x88 = (ETUnsigned, "FlagDefault")
    g 0x55AA = (ETUnsigned, "FlagForced")
    g 0x9C = (ETUnsigned, "FlagLacing")
    g 0x23E383 = (ETUnsigned, "DefaultDuration")
    g 0x6DE7 = (ETUnsigned, "MinCache")
    g 0x6DF8 = (ETUnsigned, "MaxCache")
    g 0x23314F = (ETFloat, "TrackTimecodeScale")
    g 0x537F = (ETSigned, "TrackOffset")
    g 0x536E = (ETTextUtf8, "Name")
    g 0x22B59C = (ETTextUtf8, "Language")
    g 0x86 = (ETTextAscii, "CodecID")
    g 0x63A2 = (ETTextAscii, "CodecPrivate")
    g 0x258688 = (ETTextUtf8, "CodecName")

    g 0xE0 = (ETMaster, "Video")
    g 0xB0 = (ETUnsigned, "PixelWidth")
    g 0xBA = (ETUnsigned, "PixelHeight")

    g 0x1F43B675 = (ETMaster, "Cluster")
    g 0xE7 = (ETUnsigned, "TimeCode")
    g 0xA7 = (ETUnsigned, "Position")
    g 0xA3 = (ETBinary, "SimpleBlock")
    g 0xA0 = (ETMaster, "BlockGroup")
    g 0xA1 = (ETBinary, "Block")
    g 0x9B = (ETUnsigned, "BlockDuration")
    
    g 0xEC = (ETBinary, "Void")
    g 0xBF = (ETBinary, "CRC-32")

    g 0x114D9B74 = (ETMaster, "SeekHead")
    g 0x4DBB = (ETMaster, "Seek")
    g 0x1C53BB6B = (ETMaster, "Cues")
    g 0x1941A469 = (ETMaster, "Attachments")
    g 0x1043A770 = (ETMaster, "Chapters")
    g 0x1254C367 = (ETMaster, "Tags")
    
    g 0x7373 = (ETMaster, "Tag")
    g 0x63C0 = (ETMaster, "Targets")
    g 0x67C8 = (ETMaster, "SimpleTag")
    
    g 0x63CA = (ETTextUtf8, "TargetType")
    g 0x45A3 = (ETTextUtf8, "TagName")
    g 0x4587 = (ETTextUtf8, "TagString")
    g 0x4585 = (ETBinary, "TagBinary")

    g unknown = (ETBinary, printf "%x" unknown)

getMajorBit :: (Bits a) => a -> Int
getMajorBit x
    | x == 0                 = error "getMajorBit 0?"
    | (x .&. 0x80) == 0x80   = 0
    | otherwise              = 1 + getMajorBit (shiftL x 1)

readEbmlNumber :: EbmlNumberType -> B.ByteString -> (Integer, B.ByteString)
readEbmlNumber ENUnmodified b = renStage2 additionalSize (toInteger head) tail  
    where
    head = B.head(b)
    tail = B.tail(b)
    additionalSize = getMajorBit head
    renStage2 :: Int -> Integer -> B.ByteString -> (Integer, B.ByteString)
    renStage2 0 x b = (x, b)
    renStage2 n x b = renStage2 (n-1) ((shiftL x 8) .|. (toInteger $ B.head b)) (B.tail b)
readEbmlNumber ENUnsigned b = (x2, tail)
    where
    additionalSize = getMajorBit $ B.head b
    (x, tail) = readEbmlNumber ENUnmodified b
    x2 = xor x $ shiftL (shiftR 0x80 $ additionalSize) (additionalSize * 8)
readEbmlNumber ENSigned b = (x2, tail)
    where
    additionalSize = getMajorBit $ B.head b
    (x, tail) = readEbmlNumber ENUnsigned b
    x2 = x - (2 ^ (6 + 7 * additionalSize) - 1)

readBigEndianNumber :: Bool -> B.ByteString -> Integer
readBigEndianNumber signed b = ret signed
    where
    firstbyte = B.head b
    ret False = foldl1' (\x y -> (shiftL x 8) .|. y) $ fmap toInteger $ B.unpack b
    ret True
        | firstbyte .&. 0x80 == 0x80    = ret False - 2^(8*B.length(b))
        | otherwise                     = ret False



readEbmlElementRaw :: B.ByteString -> (EbmlElementRaw, B.ByteString)
readEbmlElementRaw b = ((EbmlElementRaw id_ data_), rest)
    where
    (id_,   rest1) = readEbmlNumber ENUnmodified b
    (size, rest2) = readEbmlNumber ENUnsigned rest1
    data_ = B.take (fromInteger size) rest2
    rest = B.drop (fromInteger size) rest2

readEbmlElement :: B.ByteString -> (EbmlElement, B.ByteString)
readEbmlElement b = (EbmlElement id_ type_ name_ data_, rest_)
    where
    (EbmlElementRaw id_ data_raw, rest_) = readEbmlElementRaw b
    (type_, name_) = getElementTypeAndName id_
    data_ = interpretData type_ data_raw
    interpretData :: EbmlElementType -> B.ByteString -> EbmlElementData
    interpretData ETBinary b = EB b
    interpretData ETUnsigned b = EN $ readBigEndianNumber False b
    interpretData ETSigned b = EN $ readBigEndianNumber True b
    interpretData ETTextAscii b = ET $ Data.ByteString.Lazy.Char8.unpack b
    interpretData ETTextUtf8 b = ET $ Data.ByteString.Lazy.UTF8.toString b
    interpretData ETFloat b = EB b -- Sorry, I don't know how to read floats yet
    interpretData ETMaster b = EM $ readEbmlElements b

readEbmlElements :: B.ByteString -> [EbmlElement]
readEbmlElements b = rEE
    where
    consy = B.uncons b
    (element_, rest_) = readEbmlElement b
    rEE
        | consy == Nothing  = []
        | otherwise = (element_ : readEbmlElements rest_)


parseMkv :: B.ByteString -> IO ()
parseMkv b = (mapM (putStrLn . show) $  readEbmlElements b) >> return ()
-- printf "%x\n" $ fst (readEbmlNumber ENUnmodified b)
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
