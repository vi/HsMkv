module Mkvparse2 where

-- This module provides function to parse Matroska file
-- It does not support proper seeking or additional features, just converts 
-- ByteString to the lazy list of Mkvparse's MatroskaEvents

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
--import qualified Data.ByteString.Lazy.UTF8 as UTF8
import Data.Binary.IEEE754
import qualified Data.ByteString.Char8 
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.Text.Encoding.Error as EE
import Data.Char
import Data.Word
import Data.Bits
import Data.List 
import Data.Maybe 
import Text.Printf
import Control.Monad

import MkvTabular

-- Description of some events

data Info = Info {
     i_timecodeScale :: Integer
    ,i_muxingApplication :: T.Text
    ,i_writingApplication :: T.Text
    ,i_duration :: Double
    ,i_date :: Double
    ,i_segmentUid :: T.Text
} deriving (Show)


data TrackType = TT_Audio | TT_Video | TT_Subtitle | TT_Complex | TT_Logo | TT_Button | TT_Control | TT_Unknown Integer
    deriving (Show, Eq, Ord)



data Track = Track {
      t_type :: TrackType
    , t_number :: Integer
    , t_UID :: Integer
    , t_minCache :: Integer
    , t_codecId :: T.Text
    , t_codecPrivate :: B.ByteString
    , t_defaultDuration :: Integer
    , t_language :: T.Text
    , t_videoPixelWidth :: Integer
    , t_videoPixelHeight :: Integer
    , t_videoDisplayWidth :: Integer
    , t_videoDisplayHeight :: Integer
    , t_audioSamplingFrequency :: Integer
    , t_audioChannels :: Integer
    } deriving (Show)

data Frame = Frame {
  
} deriving (Show)

-- Events that comes from parser to user

data MatroskaEvent =
    ME_Resync |
    ME_EbmlElement MatroskaElement |
    ME_Info Info |
    ME_Tracks [Track] |
    ME_Frame [Frame] |
    ME_Noop
    deriving (Show)


-- Note: "do" blocks here are for Maybe, not for IO

data MatroskaElement = MatroskaElement {
     me_class :: ElementClass
    ,me_size :: Integer
    ,me_content :: ElementContent  
    } deriving (Show, Eq)

data ElementContent = 
        EC_Master [MatroskaElement] |
        EC_Unsigned Integer |
        EC_Signed Integer |
        EC_TextAscii T.Text |
        EC_TextUtf8 T.Text |
        EC_Binary B.ByteString |
        EC_Float Double |
        EC_Date Double
        deriving (Show, Eq)



-- State of the parsing. 
-- There can be nested ParserStates when parsing child elements.

data ParserMode = ReadEBML | HandleEBML deriving Show

data ParserState = ParserState {
     ps_buffer :: B.ByteString
    ,ps_timecode_scale :: Integer
    ,ps_mode :: ParserMode
    ,ps_element :: Maybe MatroskaElement
    ,ps_timecode :: Integer
} deriving (Show)


data EbmlNumberType = ENUnsigned | ENSigned | ENUnmodified  deriving (Show, Eq, Ord)

getMajorBit :: (Bits a) => a -> Maybe Int
getMajorBit x
    | x == 0                 = Nothing
    | (x .&. 0x80) == 0x80   = Just 0
    | otherwise              = do
        more <-  getMajorBit $ shiftL x 1
        return $ 1 + more

readEbmlNumber :: EbmlNumberType -> B.ByteString -> Maybe (Integer, B.ByteString)
readEbmlNumber ENUnmodified b = do
    (head, tail) <- B.uncons b
    additionalSize <- getMajorBit head
    renStage2 additionalSize (toInteger head) tail  
        where
        renStage2 :: Int -> Integer -> B.ByteString -> Maybe (Integer, B.ByteString)
        renStage2 0 x b = Just (x, b)
        renStage2 n x b = do
            (head2, tail2) <- B.uncons b
            renStage2 (n-1) ((shiftL x 8) .|. toInteger head2) tail2

readEbmlNumber ENUnsigned b = do
    (head, _) <- B.uncons b
    additionalSize <- getMajorBit $ head
    (x, tail) <- readEbmlNumber ENUnmodified b
    x2 <- return $ xor x $ shiftL (shiftR 0x80 $ additionalSize) (additionalSize * 8)
    return (x2, tail)

readEbmlNumber ENSigned b = do
    (head , _) <- B.uncons b
    additionalSize <- getMajorBit $ head
    (x, tail) <- readEbmlNumber ENUnsigned b
    x2 <- return $ x - (2 ^ (6 + 7 * additionalSize) - 1)
    return (x2, tail)

readBigEndianNumber :: Bool -> B.ByteString -> Maybe Integer
readBigEndianNumber signed b = do
    (head, tail) <- B.uncons b
    return $ ret signed head
    where
    ret False head = foldl1' (\x y -> (shiftL x 8) .|. y) $ fmap toInteger $ B.unpack b
    ret True head
        | head .&. 0x80 == 0x80    = ret False undefined - 2^(8*B.length(b))
        | otherwise                = ret False undefined


readXiphLacingNumber :: B.ByteString -> Maybe (Integer, B.ByteString)
readXiphLacingNumber b = do
    (head, tail) <- B.uncons b
    rXLN 0 head tail
    where
    rXLN :: Integer -> Word8 -> B.ByteString -> Maybe (Integer, B.ByteString)
    rXLN accum 255 tail = do
        (head2, tail2) <- B.uncons tail 
        rXLN (accum+255) head2 tail2
    rXLN accum x tail = Just (toInteger x + accum, tail)


{-
B.readFile "t.mkv" >>=  (\x -> return $ parseMkv x) >>= (\y -> return $ take 10 y) >>= mapM (\x -> putStrLn $ take 120 $ show x) >> return Nothing
-}




lazyBytestringToNormalBytestring = Data.ByteString.Char8.concat . B.toChunks

fromMatroskaDate :: Integer -> Double
fromMatroskaDate x = (fromInteger x) / 1000000000.0 + 978300000
-- 2001-01-01T00:00:00,000000000

interpretFloat :: B.ByteString -> Maybe Double
interpretFloat b = do
    num <- readBigEndianNumber False b
    case B.length b of
        4 -> Just $ fromRational $ toRational $ Data.Binary.IEEE754.wordToFloat (fromInteger num)
        8 -> Just                             $ Data.Binary.IEEE754.wordToDouble (fromInteger num)
        otherwise -> Nothing
        

parseElementContent :: ElementType -> B.ByteString -> Maybe ElementContent
parseElementContent ET_Unsigned = liftM EC_Unsigned . readBigEndianNumber False
parseElementContent ET_Signed   = liftM EC_Signed . readBigEndianNumber True
parseElementContent ET_Binary  = Just . EC_Binary
parseElementContent ET_Flatten = Just . EC_Binary
parseElementContent ET_Unknown = Just . EC_Binary
parseElementContent ET_TextUtf8  = Just . EC_TextUtf8  . E.decodeUtf8With EE.lenientDecode . lazyBytestringToNormalBytestring
parseElementContent ET_TextAscii = Just . EC_TextAscii . T.pack . C.unpack
parseElementContent ET_Master = Just . EC_Master . tryParseEbml
parseElementContent ET_Date = liftM (EC_Date . fromMatroskaDate) . readBigEndianNumber True
parseElementContent ET_Float = liftM EC_Float . interpretFloat



tryParseEbml1 :: B.ByteString -> Maybe (MatroskaElement, B.ByteString)
tryParseEbml1 buffer = do
    (id_,   rest1) <- readEbmlNumber ENUnmodified $ buffer
    (size, rest2) <- readEbmlNumber ENUnsigned rest1
    size2 <- return $ if (lookupElementType $ lookupElementId id_) == ET_Flatten then 0 else size
    (data_, rest3) <- return $ B.splitAt (fromInteger size2) rest2
    element <- let 
        klass = lookupElementId id_
        type_ = lookupElementType klass
        in liftM (MatroskaElement klass size2) $ parseElementContent type_ data_
    new_state <- Just $ rest3
    return (element, new_state)

tryParseEbml :: B.ByteString -> [MatroskaElement]
tryParseEbml input = unfoldr tryParseEbml1 input



-- Skip enough bytes to try more parsing
-- Gets to the next Segment, Cluster, Info or Tracks element

resync :: B.ByteString -> B.ByteString
resync b = result
    where
    segment_id = C.pack  "\x18\x53\x80\x67"
    cluster_id = C.pack  "\x1F\x43\xB6\x75"
    info_id    = C.pack  "\x15\x49\xA9\x66"
    tracks_id  = C.pack  "\x16\x54\xAE\x6B"

    dropper 0x18 = False
    dropper 0x1F = False
    dropper 0x15 = False
    dropper 0x16 = False
    dropper _ = True

    (_, next) = B.splitAt 4 b

    result = case
        b == B.empty ||
        B.isPrefixOf segment_id b ||
        B.isPrefixOf cluster_id b ||
        B.isPrefixOf tracks_id b ||
        B.isPrefixOf info_id b of
        True -> b
        False -> resync $ B.dropWhile dropper next

isThisTopLevelElementValid :: ElementClass -> Integer -> Bool
isThisTopLevelElementValid klass size = case klass of
    EE_Unknown _ -> False -- Unknown top-level (or Segment or Cluster) element? resync!
    EE_CRC32 -> size < 30
    otherwise -> case lookupElementType klass of
        ET_Binary -> True -- binary elements can be big
        ET_Master -> True -- master elements can be big and usually have long IDs
        ET_TextAscii -> size < 102400
        ET_TextUtf8 -> size < 1024000
        othersize -> size < 30 -- numeric things should be small
    
    
toHex1 :: Int -> String
toHex1 x = highChar : lowChar : []
    where
    high = (x .&. 0xF0) `shiftR` 4
    low = (x .&. 0x0F)
    toHexNibble t
        | t < 10  = chr (ord '0' + t)
        | otherwise = chr (ord 'a' + t - 10)
    highChar = toHexNibble high
    lowChar = toHexNibble low

toHex :: B.ByteString -> T.Text
toHex = T.pack . concat . (map $ toHex1 . fromInteger . toInteger) . B.unpack
    

parseMkv1 :: ParserState -> Maybe (MatroskaEvent, ParserState)
parseMkv1 state = result $ ps_mode state
    where
    maybe_element = tryParseEbml1 $ ps_buffer state
    result ReadEBML = case maybe_element of
        Nothing -> case B.empty == ps_buffer state of
            True -> Nothing -- really eof of file
            False -> -- need to resync 
                Just (ME_Resync, state {ps_buffer = resync $ ps_buffer state})
        Just (element, tail) ->
            case isThisTopLevelElementValid (me_class element) (me_size element) of
            False ->  Just (ME_Resync, state {ps_buffer = resync $ ps_buffer state})
            True  -> Just (ME_EbmlElement element, state {
                    ps_element = Just element,
                    ps_buffer  = tail, 
                    ps_mode    = HandleEBML })
    result HandleEBML = Just (msg, new_state {ps_element = Nothing, ps_mode = ReadEBML})
        where
        element = fromJust $ ps_element state
        klass =  me_class element
        (msg, new_state) = case klass of
            EE_Info -> handle_info state
            EE_Tracks -> handle_tracks state
            EE_Timecode -> handle_timecode state
            EE_Cluster -> handle_cluster state
            _          -> (ME_Noop, state)

    handle_info :: ParserState -> (MatroskaEvent, ParserState)
    handle_info state = (ME_Info info, new_state)
        where
        (MatroskaElement kl size content) = fromJust $ ps_element state
        entries = case content of
            EC_Master x -> x
            _ -> error "Internal error: SegmentInfo element is not master"
        handle_state_entry :: (ParserState, Info) -> MatroskaElement -> (ParserState, Info)
        handle_state_entry (ps, i) (MatroskaElement kl  size content ) = hse2 kl content
            where
            hse2 EE_MuxingApp (EC_TextUtf8 t)  = (ps, i{i_muxingApplication=t})
            hse2 EE_WritingApp (EC_TextUtf8 t) = (ps, i{i_writingApplication=t})
            hse2 EE_Duration   (EC_Float t)    = (ps, i{i_duration=t})
            hse2 EE_SegmentUID (EC_Binary t)   = (ps, i{i_segmentUid=toHex t})
            hse2 EE_TimecodeScale (EC_Unsigned t) = 
                (ps{ps_timecode_scale=t}, i{i_timecodeScale=t})
            hse2 _ _ = (ps, i)
        initial_info = Info {
             i_timecodeScale = ps_timecode_scale state
            ,i_muxingApplication = T.empty
            ,i_writingApplication = T.empty
            ,i_duration = 0.0
            ,i_date = 0.0
            ,i_segmentUid = T.pack ""
            }
        (new_state, info) = foldl' handle_state_entry (state, initial_info) entries
        

    handle_tracks :: ParserState -> (MatroskaEvent, ParserState)
    handle_tracks s = (ME_Noop, s)

    handle_cluster :: ParserState -> (MatroskaEvent, ParserState)
    handle_cluster s = (ME_Noop, s)    

    handle_timecode :: ParserState -> (MatroskaEvent, ParserState)
    handle_timecode s = (ME_Noop, s)

    

-- The main module function.

parseMkv :: B.ByteString -> [MatroskaEvent]
parseMkv input = events
    where
    initial_state = ParserState { 
         ps_buffer         = input
        ,ps_timecode_scale = 1000000
        ,ps_mode           = ReadEBML
        ,ps_element        = Nothing
        ,ps_timecode       = 0
        }
    events = unfoldr parseMkv1 initial_state
    


demoSequence :: IO [MatroskaEvent] 
demoSequence = B.readFile "t.mkv" >>=  (\x -> return $ parseMkv x) 

demoHandler :: MatroskaEvent -> IO ()
demoHandler ME_Resync = printf "Resync\n"
demoHandler (ME_EbmlElement (MatroskaElement klass size content)) = printf " %s %d\n" (show klass) size
demoHandler (ME_Info inf) =  do
    printf "Info\n"
    printf "  Timecode scale: %d\n" $ i_timecodeScale inf
    printf "  Muxing application: %s\n" $ T.unpack $ i_muxingApplication inf
    printf "  Writing application: %s\n" $ T.unpack $ i_writingApplication inf
    printf "  Duration: %g\n" $ i_duration inf
    printf "  Segment UID: %s\n" $ T.unpack $ i_segmentUid inf

demoHandler _ = return ()

demo = do
    s <- demoSequence
    mapM_ demoHandler (take 30 s)


