module Main where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8
import qualified Data.ByteString.Lazy.UTF8
import Data.Bits
import Data.List -- foldl1'
import Data.Maybe -- catMaybes

import System.IO
import Text.Printf

type EbmlElementID = Integer
type EbmlElementName = String
type TrackNumber = Integer
type TimeCode = Double
type TimeScale = Integer
type Duration = Double

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

data MatroskaFrame = MatroskaFrame {
    trackNumber :: TrackNumber,
    timeCode :: TimeCode,
    frameData :: B.ByteString,
    moreLacedFrames :: Int,
    duration :: Maybe Duration
    -- element :: EbmlElement
    } deriving (Show)

data MatroskaEvent = 
    TracksAvailable [EbmlElement] |
    SegmentInfoAvailable [EbmlElement] |
    FrameReceived MatroskaFrame |
    DebugEvent String
    deriving (Show)


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

data MatroskaCluster = MatroskaCluster TimeCode [(B.ByteString, Maybe Duration)] deriving (Show)

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


readXiphLacingNumber :: B.ByteString -> (Integer, B.ByteString)
readXiphLacingNumber b = rXLN 0 b
    where
    rXLN accum b = case B.head b of
        255 -> rXLN (accum+255) $ B.tail b
        x -> ((toInteger x) + accum, B.tail b)


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

parseMkv :: B.ByteString -> [MatroskaEvent]
parseMkv b = result
    where
    root_level_elements = readEbmlElements b
    segment = case find (\x -> eeId x == 0x18538067) root_level_elements of
        Nothing -> error "No Segment root-level element found in the file"
        Just x -> x

    getChildren (EM x) = x
    getChildren _ = error "mkvparse.hs: Expected master element with children"

    -- handle child of Segment
    handleElement :: EbmlElement -> [MatroskaEvent]
    handleElement x
        | eeId x == 0x1549A966  = [SegmentInfoAvailable $ getChildren $ eeData x]
        | eeId x == 0x1654AE6B  = [TracksAvailable $ getChildren $ eeData x]
        | eeId x == 0x1F43B675  = handleCluster $ parseCluster x
    handleElement x = [DebugEvent $ show x]

    parseCluster :: EbmlElement -> MatroskaCluster
    parseCluster (EbmlElement _ ETMaster _ (EM nodes)) = MatroskaCluster tc_conv frames
        where
        tc = case find (\x -> eeId x == 0xE7) nodes of
            Just (EbmlElement _ _ _ (EN x)) -> x
            _                          -> error "Cluster without a valid timecode"

        tc_conv = (fromInteger tc :: Double) * 0.000000001 * 1000000

        extractIfABlock :: EbmlElement -> Maybe (B.ByteString, Maybe Duration)
        extractIfABlock (EbmlElement 0xA3 ETBinary _ (EB buf)) = Just (buf, Nothing)
        -- Add support for blocks with duration here
        extractIfABlock _ = Nothing

        frames = catMaybes $ fmap extractIfABlock nodes
    parseCluster _ = error "Malformed cluster"

    handleCluster :: MatroskaCluster -> [MatroskaEvent]
    handleCluster (MatroskaCluster tc frames) = concat $ fmap (handleFrame 1000000 tc) frames

    handleFrame :: TimeScale -> TimeCode -> (B.ByteString, Maybe Duration) -> [MatroskaEvent]
    handleFrame tscale segment_timecode (buf, dur) = [FrameReceived frame]
        where
        (track_number, rest1) = readEbmlNumber ENUnsigned buf
        (rel_timecode, rest2) = (readBigEndianNumber True (B.take 2 rest1), B.drop 2 rest1)
        (flags, rest3) = (readBigEndianNumber False (B.take 1 rest2), B.drop 1 rest2)
        content = case flags .&. 0x60 of
            0x00 -> rest3
            _ -> rest3

        timecode = segment_timecode + 0.000000001 * (fromInteger (tscale * rel_timecode)::Double)
        frame = MatroskaFrame track_number timecode content 0 dur
        

    segmentChildren = getChildren $ eeData segment 
    result =  concat $ fmap handleElement $ segmentChildren
-- (mapM (putStrLn . show) $  readEbmlElements b) >> return ()
-- printf "%x\n" $ fst (readEbmlNumber ENUnmodified b)
-- parseMkv _ = return ()


main :: IO ()
main = do
    contents <- B.hGetContents System.IO.stdin
    mapM_ putStrLn $ map show $ parseMkv contents
    -- B.readFile "q.mkv" >>= \contents -> B.writeFile "qq.mkv" contents
    {- h <- openFile "q.mkv" ReadMode
    ho <- openFile "qq.mkv" WriteMode
    hSetBuffering h (BlockBuffering $ Just 1024)
    hSetBuffering ho (BlockBuffering $ Just 1024)
    content <- B.hGetContents h
    B.hPutStr ho content -}
