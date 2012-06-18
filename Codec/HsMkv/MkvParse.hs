module Codec.HsMkv.MkvParse (
     parseMkv
    ,Info(..)
    ,TrackType(..)
    ,Track(..)
    ,Frame(..)
    ,MatroskaEvent(..)

    ,MatroskaElement(..)
    ,ElementContent(..)
    ,MatroskaLacingType(..)
    ,EbmlNumberType(..)
    ,readEbmlNumber
    ,readBigEndianNumber
    ,readXiphLacingNumber
    ,fromMatroskaDate
    ,tryParseEbml1
    ,tryParseEbml
    ,resync
    ,ebmlChildren
    ,parse_lacing -- May change
    ,toHex
    ) where

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

import Codec.HsMkv.MkvTabular

-- Description of some events

data Info = Info {
     i_timecodeScale :: Integer
    ,i_muxingApplication :: Maybe T.Text
    ,i_writingApplication :: Maybe T.Text
    ,i_duration :: Maybe Double
    ,i_date :: Maybe Double
    ,i_segmentUid :: Maybe T.Text -- hex encoded
    ,i_title :: Maybe T.Text
} deriving (Show)


data TrackType = 
    TT_Audio | 
    TT_Video | 
    TT_Subtitle | 
    TT_Complex | 
    TT_Logo | 
    TT_Button | 
    TT_Control | 
    TT_Unknown Integer
    deriving (Show, Eq, Ord)



data Track = Track {
      t_type :: TrackType
    , t_number :: Integer
    , t_codecId :: T.Text

    , t_UID :: Maybe Integer
    , t_minCache :: Maybe Integer
    , t_codecPrivate :: Maybe B.ByteString
    , t_defaultDuration :: Maybe Double
    , t_language :: Maybe T.Text
    , t_videoPixelWidth :: Maybe Integer
    , t_videoPixelHeight :: Maybe Integer
    , t_videoDisplayWidth :: Maybe Integer
    , t_videoDisplayHeight :: Maybe Integer
    , t_audioSamplingFrequency :: Maybe Double
    , t_audioOutputSamplingFrequency :: Maybe Double
    , t_audioChannels :: Maybe Integer
    } deriving (Show)

data Frame = Frame {
     f_trackNumber :: Integer
    ,f_timeCode :: Double
    ,f_data :: [B.ByteString]
    ,f_duration :: Maybe Double
    ,f_invisible :: Bool
    ,f_discardable :: Bool
    ,f_keyframe :: Bool
} deriving (Show)

data MatroskaEvent =
    ME_Frame Frame |
    ME_Tracks [Track] |
    ME_Info Info |

    ME_EbmlElement MatroskaElement |
    ME_Resync |
    ME_Noop
    deriving (Show)



data MatroskaElement = MatroskaElement {
     me_class :: ElementClass
    ,me_size :: Maybe Integer
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
data MatroskaLacingType = NoLacing | XiphLacing | FixedSizeLacing | EbmlLacing deriving (Show)

data ParserState = ParserState {
     ps_buffer :: B.ByteString
    ,ps_timecode_scale :: Integer
    ,ps_mode :: ParserMode
    ,ps_element :: Maybe MatroskaElement
    ,ps_timecode :: Integer
} deriving (Show)


data EbmlNumberType = ENUnsigned | ENSigned | ENUnmodified  deriving (Show, Eq, Ord)

-- Note: "do" blocks here are for Maybe, not for IO

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
        in liftM (MatroskaElement klass (Just size2)) $ parseElementContent type_ data_
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

isThisTopLevelElementValid :: ElementClass -> Maybe Integer -> Bool
isThisTopLevelElementValid klass (Just size) = case klass of
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
    
ebmlChildren :: MatroskaElement -> [MatroskaElement]
ebmlChildren (MatroskaElement _ _ (EC_Master ret)) = ret
ebmlChildren _ = []

-- parse_lacing function takes the content (starting from the number of laced frames) 
-- and return de-laced frames together with decreasing numbers of remaining frames
-- 
-- To be refacroted: handling of bad data
parse_lacing :: MatroskaLacingType -> B.ByteString -> [B.ByteString]
parse_lacing ltype buf = result
    where
    len = (fromIntegral $ B.length buf) :: Int
    (num_laced_frames_i, rest1) = ((fromJust $ readBigEndianNumber False (B.take 1 buf))+1, B.drop 1 buf)
    num_laced_frames = case ltype of
        NoLacing -> 1
        _        -> fromInteger num_laced_frames_i :: Int
    (lengths, rest2) = case ltype of
        NoLacing -> ([len], buf)
        XiphLacing -> readXiphLengths 0 num_laced_frames rest1
        EbmlLacing -> readEbmlLengths True 0 0 num_laced_frames rest1
        FixedSizeLacing -> (
                take num_laced_frames $ repeat $ (fromIntegral $ B.length rest2) `div` num_laced_frames,
                rest1
                )

    readXiphLengths :: Int -> Int -> B.ByteString -> ([Int], B.ByteString)
    --                 accumulated length -> more subframes -> buffer -> (length list, data_after_lengths)
    readXiphLengths acc 1 b = ([len - acc], b)
    readXiphLengths acc n b = ((thislen:tail), finalrest)
        where 
        (tail, finalrest) = readXiphLengths (acc+thislen) (n-1) rest
        (thislen_i, rest) = fromJust $ readXiphLacingNumber b
        thislen = fromInteger thislen_i :: Int

    readEbmlLengths :: Bool -> Int -> Int -> Int -> B.ByteString -> ([Int], B.ByteString)
    --  first_subframe? -> prevous_length -> accumulated length -> more subframes -> buffer -> (length list, data_after_lengths)
    readEbmlLengths fst prev acc 1 b = ([len - acc], b)
    readEbmlLengths fst prev acc n b = ((thislen:tail), finalrest)
        where 
        nt = if fst then ENUnsigned else ENSigned
        (thislen_i, rest) = fromJust $ readEbmlNumber nt b
        thislen = (fromInteger thislen_i :: Int) + prev
        (tail, finalrest) = readEbmlLengths False thislen (acc+thislen) (n-1) rest

    subframes (len:lengths) buf more_laced_frames = head : tail
        where
        head = B.take (fromIntegral len) buf
        tail = subframes lengths (B.drop (fromIntegral len) buf) (more_laced_frames - 1)
    subframes [] _ _ = []
    result = subframes lengths rest2 (num_laced_frames-1)


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
            EE_BlockGroup -> handle_blockGroup state
            EE_SimpleBlock -> handle_simpleBlock state
            _          -> (ME_Noop, state)

    handle_info :: ParserState -> (MatroskaEvent, ParserState)
    handle_info state = (ME_Info info, new_state)
        where
        entries = ebmlChildren $ fromJust $ ps_element state
        handle_info_entry :: (ParserState, Info) -> MatroskaElement -> (ParserState, Info)
        handle_info_entry (ps, i) (MatroskaElement kl  size content ) = hie2 kl content
            where
            hie2 EE_MuxingApp (EC_TextUtf8 t)  = (ps, i{i_muxingApplication=Just t})
            hie2 EE_WritingApp (EC_TextUtf8 t) = (ps, i{i_writingApplication=Just t})
            hie2 EE_Duration   (EC_Float t)    = (ps, i{i_duration=Just t})
            hie2 EE_SegmentUID (EC_Binary t)   = (ps, i{i_segmentUid=Just $ toHex t})
            hie2 EE_Title (EC_TextUtf8 t)      = (ps, i{i_title=Just t})
            hie2 EE_TimecodeScale (EC_Unsigned t) = 
                (ps{ps_timecode_scale=t}, i{i_timecodeScale=t})
            hie2 _ _ = (ps, i)
        initial_info = Info {
             i_timecodeScale = ps_timecode_scale state
            ,i_muxingApplication = Nothing
            ,i_writingApplication = Nothing
            ,i_duration = Nothing
            ,i_date = Nothing
            ,i_segmentUid = Nothing
            ,i_title = Nothing
            }
        (new_state, info) = foldl' handle_info_entry (state, initial_info) entries
        

    handle_tracks :: ParserState -> (MatroskaEvent, ParserState)
    handle_tracks s = (ME_Tracks tracks, s)
        where
        entries = ebmlChildren $ fromJust $ ps_element state
        handle_track_entry :: Track -> MatroskaElement -> Track
        handle_track_entry i (MatroskaElement kl size content ) = hte2 kl content
            where
            hte2 EE_TrackNumber (EC_Unsigned t)  = i{t_number=t}
            hte2 EE_TrackUID (EC_Unsigned t)  = i{t_UID=Just t}
            hte2 EE_CodecID (EC_TextAscii t)  = i{t_codecId=t}
            hte2 EE_CodecPrivate (EC_Binary t) = i{t_codecPrivate=Just t}
            hte2 EE_DefaultDuration (EC_Unsigned t) = 
                i{t_defaultDuration=Just $ fromInteger t / 1000000000.0}
            hte2 EE_MinCache (EC_Unsigned t)  = i{t_minCache=Just t}
            hte2 EE_Language (EC_TextAscii t) = i{t_language=Just t}
            hte2 EE_Video (EC_Master t) = foldl' hte2_video i t
                where
                hte2_video j (MatroskaElement EE_PixelWidth    _ (EC_Unsigned t)) = j{t_videoPixelWidth = Just t}
                hte2_video j (MatroskaElement EE_PixelHeight   _ (EC_Unsigned t)) = j{t_videoPixelHeight = Just t}
                hte2_video j (MatroskaElement EE_DisplayWidth  _ (EC_Unsigned t)) = j{t_videoDisplayWidth = Just t}
                hte2_video j (MatroskaElement EE_DisplayHeight _ (EC_Unsigned t)) = j{t_videoDisplayHeight = Just t}
                hte2_video j _ = j
            hte2 EE_Audio (EC_Master t) = foldl' hte2_audio i t
                where
                hte2_audio j (MatroskaElement EE_SamplingFrequency  _ (EC_Float t))    
                    = j{t_audioSamplingFrequency = Just t}
                hte2_audio j (MatroskaElement EE_OutputSamplingFrequency _ (EC_Float t))
                    = j{t_audioOutputSamplingFrequency = Just t}
                hte2_audio j (MatroskaElement EE_Channels           _ (EC_Unsigned t))
                    = j{t_audioChannels = Just t}
                hte2_audio j _ = j
            hte2 EE_TrackType (EC_Unsigned t) = i{t_type = interpret_tt t}
            hte2 _ _ = i
        interpret_tt 1 = TT_Video
        interpret_tt 2 = TT_Audio
        interpret_tt 3 = TT_Complex
        interpret_tt 0x10 = TT_Logo
        interpret_tt 0x11 = TT_Subtitle
        interpret_tt 0x12 = TT_Button
        interpret_tt 0x20 = TT_Control
        initial_track = Track {
             t_number = -1
            ,t_type = TT_Unknown $ -1
            ,t_codecId = T.empty

            ,t_UID = Nothing
            ,t_codecPrivate = Nothing
            ,t_defaultDuration = Nothing
            ,t_minCache = Nothing
            ,t_language = Nothing
            ,t_videoPixelWidth = Nothing
            ,t_videoPixelHeight = Nothing
            ,t_videoDisplayWidth = Nothing
            ,t_videoDisplayHeight = Nothing
            ,t_audioSamplingFrequency = Nothing
            ,t_audioOutputSamplingFrequency = Nothing
            ,t_audioChannels = Nothing
            }
        handle_one_track :: MatroskaElement -> Track
        handle_one_track (MatroskaElement _ _ (EC_Master x)) =
            foldl' handle_track_entry initial_track x  
        handle_one_track _ = initial_track -- hack to prevent errors on invalid files

        tracks = map handle_one_track entries


    handle_simpleBlock :: ParserState -> (MatroskaEvent, ParserState)
    handle_simpleBlock s = (msg, s)    
        where
        buf = (\(EC_Binary x) -> x) $ me_content $ fromJust $ ps_element s
        msg = case handle_frame Nothing buf of
            Nothing -> ME_Noop
            Just f -> ME_Frame f
            
    
    handle_blockGroup :: ParserState -> (MatroskaEvent, ParserState)
    handle_blockGroup s = (msg, s)    
        where
        entries = ebmlChildren $ fromJust $ ps_element state
        durationEl = find (\x -> EE_BlockDuration == me_class x) entries
        duration :: Maybe Integer
        duration = durationEl >>= (\(MatroskaElement _ _ (EC_Unsigned x)) -> return x)
        blockEl = find (\x -> EE_Block == me_class x) entries
        buf = blockEl >>= (\(MatroskaElement _ _ (EC_Binary x)) -> return x)
        msg = case buf >>= handle_frame duration of
            Nothing -> ME_Noop
            Just f -> ME_Frame f

    --              duration         block data
    handle_frame :: Maybe Integer  ->  B.ByteString -> Maybe Frame
    handle_frame dur buf = do
        (track_number, rest1) <- readEbmlNumber ENUnsigned buf
        (rel_timcode_buf, rest2) <- return $ B.splitAt 2 rest1
        rel_timecode <- readBigEndianNumber True rel_timcode_buf
        (flags, rest3) <- B.uncons rest2
        return $ handleFrame' track_number rel_timecode flags rest3
        where
        handleFrame' track_number rel_timecode flags rest3 = frame
            where
            contents :: [B.ByteString] -- subframe and number of remaining subframes in the lace
            contents = case flags .&. 0x06 of
                0x00 -> parse_lacing NoLacing rest3 --  equals to [(rest3, 0)]
                0x02 -> parse_lacing XiphLacing rest3
                0x04 -> parse_lacing FixedSizeLacing rest3
                0x06 -> parse_lacing EbmlLacing rest3


            tscale           = ps_timecode_scale state
            cluster_timecode = ps_timecode state
            timecode = cluster_timecode + rel_timecode
            timecodeToSeconds x = (fromInteger (x*tscale)::Double)*0.000000001
            timecodeSeconds = timecodeToSeconds timecode
            flag_discardable = testBit flags 0
            flag_invisible   = testBit flags 3
            flag_keyframe    = testBit flags 7
            frame = Frame {
                 f_trackNumber = track_number
                ,f_timeCode = timecodeSeconds
                ,f_duration = liftM timecodeToSeconds  dur
                ,f_data = contents
                ,f_discardable = flag_discardable
                ,f_keyframe = flag_keyframe
                ,f_invisible = flag_invisible
                }
        

    handle_timecode :: ParserState -> (MatroskaEvent, ParserState)
    handle_timecode s = (ME_Noop, new_state)
        where
        (MatroskaElement kl size content) = fromJust $ ps_element state
        timecode = case content of
            EC_Unsigned x -> x
            otherwise -> error "Internal error: Timecode element is not unsigned"
        new_state = s{ps_timecode = timecode}
        

    

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
    
