module Codec.HsMkv.MkvParse (
     parseMkv
    ,readEbmlNumber
    ,readBigEndianNumber
    ,readXiphLacingNumber
    ,fromMatroskaDate
    ,tryParseEbml1
    ,tryParseEbml
    ,resync
    ,ebmlChildren
    ,parseLacing -- May change
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
import qualified Data.ByteString
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.Text.Encoding.Error as EE
import Data.Char
import Data.Word
import Data.Bits
import Data.List 
import Data.Maybe 
import Control.Monad

import Codec.HsMkv.MkvTabular
import Codec.HsMkv.Model


data ParserState = ParserState {
     psBuffer :: B.ByteString
    ,psTimecodeScale :: Integer
    ,psMode :: ParserMode
    ,psElement :: Maybe MatroskaElement
    ,psTimecode :: Integer
} deriving (Show)

data ParserMode = ReadEBML | HandleEBML deriving Show


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
    (head1, tail1) <- B.uncons b
    additionalSize <- getMajorBit head1
    renStage2 additionalSize (fromIntegral head1) tail1
        where
        renStage2 :: Int -> Integer -> B.ByteString -> Maybe (Integer, B.ByteString)
        renStage2 0 x rest = Just (x, rest)
        renStage2 n x rest = do
            (head2, tail2) <- B.uncons rest
            renStage2 (n-1) (shiftL x 8 .|. fromIntegral head2) tail2

readEbmlNumber ENUnsigned b = do
    (head1, _) <- B.uncons b
    additionalSize <- getMajorBit head1
    (x, tail1) <- readEbmlNumber ENUnmodified b
    let x2 = xor x $ shiftL (shiftR 0x80 additionalSize) (additionalSize * 8)
    return (x2, tail1)

readEbmlNumber ENSigned b = do
    (head1 , _) <- B.uncons b
    additionalSize <- getMajorBit head1
    (x, tail1) <- readEbmlNumber ENUnsigned b
    let x2 = x - (2 ^ (6 + 7 * additionalSize) - 1)
    return (x2, tail1)

readBigEndianNumber :: Bool -> B.ByteString -> Maybe Integer
readBigEndianNumber signed b = do
    (head1, _) <- B.uncons b -- just check that it is not empty
    return $ ret signed head1
    where
    ret False _ = foldl1' (\x y -> shiftL x 8 .|. y) $ fmap fromIntegral $ B.unpack b
    ret True head1
        | head1 .&. 0x80 == 0x80    = ret False undefined - 2^(8*B.length b)
        | otherwise                = ret False undefined


readXiphLacingNumber :: B.ByteString -> Maybe (Integer, B.ByteString)
readXiphLacingNumber b = do
    (head1, tail1) <- B.uncons b
    rXLN 0 head1 tail1
    where
    rXLN :: Integer -> Word8 -> B.ByteString -> Maybe (Integer, B.ByteString)
    rXLN accum 255 tail1 = do
        (head2, tail2) <- B.uncons tail1 
        rXLN (accum+255) head2 tail2
    rXLN accum x tail1 = Just (fromIntegral x + accum, tail1)


{-
B.readFile "t.mkv" >>=  (\x -> return $ parseMkv x) >>= (\y -> return $ take 10 y) >>= mapM (\x -> putStrLn $ take 120 $ show x) >> return Nothing
-}



lazyBytestringToNormalBytestring :: B.ByteString -> Data.ByteString.ByteString
lazyBytestringToNormalBytestring = Data.ByteString.Char8.concat . B.toChunks

fromMatroskaDate :: Integer -> Double
fromMatroskaDate x = fromIntegral x / 1000000000.0 + 978300000
-- 2001-01-01T00:00:00,000000000

interpretFloat :: B.ByteString -> Maybe Double
interpretFloat b = do
    num <- readBigEndianNumber False b
    case B.length b of
        4 -> Just $ fromRational $ toRational $ Data.Binary.IEEE754.wordToFloat (fromIntegral num)
        8 -> Just                             $ Data.Binary.IEEE754.wordToDouble (fromIntegral num)
        _ -> Nothing
        

parseElementContent :: ElementType -> B.ByteString -> Maybe ElementContent
parseElementContent ETUnsigned = liftM ECUnsigned . readBigEndianNumber False
parseElementContent ETSigned   = liftM ECSigned . readBigEndianNumber True
parseElementContent ETBinary  = Just . ECBinary
parseElementContent ETFlatten = Just . ECBinary
parseElementContent ETUnknown = Just . ECBinary
parseElementContent ETTextUtf8  = Just . ECTextUtf8  . E.decodeUtf8With EE.lenientDecode . lazyBytestringToNormalBytestring
parseElementContent ETTextAscii = Just . ECTextAscii . T.pack . C.unpack
parseElementContent ETMaster = Just . ECMaster . tryParseEbml
parseElementContent ETDate = liftM (ECDate . fromMatroskaDate) . readBigEndianNumber True
parseElementContent ETFloat = liftM ECFloat . interpretFloat



tryParseEbml1 :: B.ByteString -> Maybe (MatroskaElement, B.ByteString)
tryParseEbml1 buffer = do
    (id_,   rest1) <- readEbmlNumber ENUnmodified buffer
    (size, rest2) <- readEbmlNumber ENUnsigned rest1
    let size2 = if lookupElementType (lookupElementId id_) == ETFlatten then 0 else size
    let (data_, rest3) = B.splitAt (fromIntegral size2) rest2
    element <- let 
        klass = lookupElementId id_
        type_ = lookupElementType klass
        in liftM (MatroskaElement klass (Just size2)) $ parseElementContent type_ data_
    new_state <- Just rest3
    return (element, new_state)

tryParseEbml :: B.ByteString -> [MatroskaElement]
tryParseEbml = unfoldr tryParseEbml1



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

    (_, next) = B.splitAt 1 b

    result = if
            b == B.empty ||
            B.isPrefixOf segment_id b ||
            B.isPrefixOf cluster_id b ||
            B.isPrefixOf tracks_id b ||
            B.isPrefixOf info_id b
        then b
        else resync $ B.dropWhile dropper next

isThisTopLevelElementValid :: ElementClass -> Maybe Integer -> Bool
isThisTopLevelElementValid klass (Just size) = case klass of
    EEUnknown _ -> False -- Unknown top-level (or Segment or Cluster) element? resync!
    EECRC32 -> size < 30
    _ -> case lookupElementType klass of
        ETBinary -> True -- binary elements can be big
        ETMaster -> True -- master elements can be big and usually have long IDs
        ETTextAscii -> size < 102400
        ETTextUtf8 -> size < 1024000
        ETFlatten -> True
        _ -> size < 30 -- numeric things should be small
isThisTopLevelElementValid _ Nothing = False -- should not happen
    
    
toHex1 :: Int -> String
toHex1 x = [highChar, lowChar]
    where
    high = (x .&. 0xF0) `shiftR` 4
    low =   x .&. 0x0F
    toHexNibble t
        | t < 10  = chr (ord '0' + t)
        | otherwise = chr (ord 'a' + t - 10)
    highChar = toHexNibble high
    lowChar = toHexNibble low

toHex :: B.ByteString -> T.Text
toHex = T.pack . concatMap (toHex1 . fromIntegral . fromIntegral) . B.unpack
    
ebmlChildren :: MatroskaElement -> [MatroskaElement]
ebmlChildren (MatroskaElement _ _ (ECMaster ret)) = ret
ebmlChildren _ = []

-- parseLacing function takes the content (starting from the number of laced frames) 
-- and return de-laced frames together with decreasing numbers of remaining frames
-- 
-- To be refacroted: handling of bad data
parseLacing :: MatroskaLacingType -> B.ByteString -> [B.ByteString]
parseLacing ltype buf = result
    where
    len = (fromIntegral $ B.length buf) :: Int
    (num_laced_frames_i, rest1) = (fromJust (readBigEndianNumber False (B.take 1 buf))+1, B.drop 1 buf)
    num_laced_frames = case ltype of
        NoLacing -> 1
        _        -> fromIntegral num_laced_frames_i :: Int
    (lengths, rest2) = case ltype of
        NoLacing -> ([len], buf)
        XiphLacing -> readXiphLengths 0 num_laced_frames rest1
        EbmlLacing -> readEbmlLengths True 0 0 num_laced_frames rest1
        FixedSizeLacing -> (
                replicate num_laced_frames $ fromIntegral (B.length rest2) `div` num_laced_frames,
                rest1
                )

    readXiphLengths :: Int -> Int -> B.ByteString -> ([Int], B.ByteString)
    --                 accumulated length -> more subframes -> buffer -> (length list, data_after_lengths)
    readXiphLengths acc 1 b = ([len - acc], b)
    readXiphLengths acc n b = (thislen:tail1, finalrest)
        where 
        (tail1, finalrest) = readXiphLengths (acc+thislen) (n-1) rest
        (thislen_i, rest) = fromJust $ readXiphLacingNumber b
        thislen = fromIntegral thislen_i :: Int

    readEbmlLengths :: Bool -> Int -> Int -> Int -> B.ByteString -> ([Int], B.ByteString)
    --                 first_subframe? -> prevous_length -> accumulated length -> more subframes -> buffer -> (length list, data_after_lengths)
    readEbmlLengths _        _    acc 1 b = ([len - acc], b)
    readEbmlLengths is_first prev acc n b = (thislen:tail1, finalrest)
        where 
        number_type = if is_first then ENUnsigned else ENSigned
        (thislen_i, rest) = fromJust $ readEbmlNumber number_type b
        thislen = (fromIntegral thislen_i :: Int) + prev
        (tail1, finalrest) = readEbmlLengths False thislen (acc+thislen) (n-1) rest

    subframes (len_:lengths2) buf2 more_laced_frames = head1 : tail1
        where
        head1 = B.take (fromIntegral len_) buf2
        tail1 = subframes lengths2 (B.drop (fromIntegral len_) buf2) (more_laced_frames - 1)
    subframes [] _ _ = []
    result = subframes lengths rest2 (num_laced_frames-1)


parseMkv1 :: ParserState -> Maybe (MatroskaEvent, ParserState)
parseMkv1 state = result $ psMode state
    where
    maybe_element = tryParseEbml1 $ psBuffer state
    result ReadEBML = case maybe_element of
        Nothing -> if B.empty == psBuffer state
            then Nothing -- really eof of file
            else -- need to resync 
                Just (MEResync, state {psBuffer = resync $ psBuffer state})
        Just (element, tail1) ->
            if not $ isThisTopLevelElementValid (meClass element) (meSize element)
                then Just (MEResync, state {psBuffer = resync $ psBuffer state})
                else Just (MEEbmlElement element, state {
                    psElement = Just element,
                    psBuffer  = tail1, 
                    psMode    = HandleEBML })
    result HandleEBML = Just (msg, new_state {psElement = Nothing, psMode = ReadEBML})
        where
        element = fromJust $ psElement state
        klass =  meClass element
        (msg, new_state) = case klass of
            EEInfo -> handle_info
            EETracks -> handle_tracks
            EETimecode -> handle_timecode
            EEBlockGroup -> handle_blockGroup
            EESimpleBlock -> handle_simpleBlock
            _          -> (MENoop, state)

    handle_info :: (MatroskaEvent, ParserState)
    handle_info = (MEInfo info, new_state)
        where
        entries = ebmlChildren $ fromJust $ psElement state
        handle_info_entry :: (ParserState, Info) -> MatroskaElement -> (ParserState, Info)
        handle_info_entry (ps, i) (MatroskaElement kl  _ content ) = hie2 kl content
            where
            hie2 EEMuxingApp (ECTextUtf8 t)  = (ps, i{iMuxingApplication=Just t})
            hie2 EEWritingApp (ECTextUtf8 t) = (ps, i{iWritingApplication=Just t})
            hie2 EEDuration   (ECFloat t)    = (ps, i{iDuration=Just t})
            hie2 EESegmentUID (ECBinary t)   = (ps, i{iSegmentUid=Just $ toHex t})
            hie2 EETitle (ECTextUtf8 t)      = (ps, i{iTitle=Just t})
            hie2 EETimecodeScale (ECUnsigned t) = 
                (ps{psTimecodeScale=t}, i{iTimecodeScale=t})
            hie2 _ _ = (ps, i)
        initial_info = Info {
             iTimecodeScale = psTimecodeScale state
            ,iMuxingApplication = Nothing
            ,iWritingApplication = Nothing
            ,iDuration = Nothing
            ,iDate = Nothing
            ,iSegmentUid = Nothing
            ,iTitle = Nothing
            }
        (new_state, info) = foldl' handle_info_entry (state, initial_info) entries
        

    handle_tracks :: (MatroskaEvent, ParserState)
    handle_tracks = (METracks tracks, state)
        where
        entries = ebmlChildren $ fromJust $ psElement state
        handle_track_entry :: Track -> MatroskaElement -> Track
        handle_track_entry i (MatroskaElement kl _ content ) = hte2 kl content
            where
            hte2 EETrackNumber (ECUnsigned t)  = i{tNumber=t}
            hte2 EETrackUID (ECUnsigned t)  = i{tUID=Just t}
            hte2 EECodecID (ECTextAscii t)  = i{tCodecId=t}
            hte2 EECodecPrivate (ECBinary t) = i{tCodecPrivate=Just t}
            hte2 EEDefaultDuration (ECUnsigned t) = 
                i{tDefaultDuration=Just $ fromIntegral t / 1000000000.0}
            hte2 EEMinCache (ECUnsigned t)  = i{tMinCache=Just t}
            hte2 EELanguage (ECTextAscii t) = i{tLanguage=Just t}
            hte2 EEVideo (ECMaster t) = foldl' hte2_video i t
                where
                hte2_video j (MatroskaElement EEPixelWidth    _ (ECUnsigned d)) = j{tVideoPixelWidth = Just d}
                hte2_video j (MatroskaElement EEPixelHeight   _ (ECUnsigned d)) = j{tVideoPixelHeight = Just d}
                hte2_video j (MatroskaElement EEDisplayWidth  _ (ECUnsigned d)) = j{tVideoDisplayWidth = Just d}
                hte2_video j (MatroskaElement EEDisplayHeight _ (ECUnsigned d)) = j{tVideoDisplayHeight = Just d}
                hte2_video j _ = j
            hte2 EEAudio (ECMaster t) = foldl' hte2_audio i t
                where
                hte2_audio j (MatroskaElement EESamplingFrequency       _ (ECFloat d))    
                    = j{tAudioSamplingFrequency       = Just d}
                hte2_audio j (MatroskaElement EEOutputSamplingFrequency _ (ECFloat d))
                    = j{tAudioOutputSamplingFrequency = Just d}
                hte2_audio j (MatroskaElement EEChannels                _ (ECUnsigned d))
                    = j{tAudioChannels                = Just d}
                hte2_audio j _ = j
            hte2 EETrackType (ECUnsigned t) = i{tType = interpret_tt t}
            hte2 _ _ = i
        interpret_tt 1 = TTVideo
        interpret_tt 2 = TTAudio
        interpret_tt 3 = TTComplex
        interpret_tt 0x10 = TTLogo
        interpret_tt 0x11 = TTSubtitle
        interpret_tt 0x12 = TTButton
        interpret_tt 0x20 = TTControl
        interpret_tt x = TTUnknown x
        initial_track = Track {
             tNumber = -1
            ,tType = TTUnknown $ -1
            ,tCodecId = T.empty

            ,tUID = Nothing
            ,tCodecPrivate = Nothing
            ,tDefaultDuration = Nothing
            ,tMinCache = Nothing
            ,tLanguage = Nothing
            ,tVideoPixelWidth = Nothing
            ,tVideoPixelHeight = Nothing
            ,tVideoDisplayWidth = Nothing
            ,tVideoDisplayHeight = Nothing
            ,tAudioSamplingFrequency = Nothing
            ,tAudioOutputSamplingFrequency = Nothing
            ,tAudioChannels = Nothing
            }
        handle_one_track :: MatroskaElement -> Track
        handle_one_track (MatroskaElement _ _ (ECMaster x)) =
            foldl' handle_track_entry initial_track x  
        handle_one_track _ = initial_track -- hack to prevent errors on invalid files

        tracks = map handle_one_track entries


    handle_simpleBlock :: (MatroskaEvent, ParserState)
    handle_simpleBlock = (msg, state)    
        where
        buf = (\(ECBinary x) -> x) $ meContent $ fromJust $ psElement state
        msg = case handle_frame Nothing buf of
            Nothing -> MENoop
            Just f -> MEFrame f
            
    
    handle_blockGroup :: (MatroskaEvent, ParserState)
    handle_blockGroup = (msg, state)    
        where
        entries = ebmlChildren $ fromJust $ psElement state
        durationEl = find (\x -> EEBlockDuration == meClass x) entries
        duration :: Maybe Integer
        duration = durationEl >>= (\(MatroskaElement _ _ (ECUnsigned x)) -> return x)
        blockEl = find (\x -> EEBlock == meClass x) entries
        buf = blockEl >>= (\(MatroskaElement _ _ (ECBinary x)) -> return x)
        msg = case buf >>= handle_frame duration of
            Nothing -> MENoop
            Just f -> MEFrame f

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
                0x00 -> parseLacing NoLacing rest3 --  equals to [(rest3, 0)]
                0x02 -> parseLacing XiphLacing rest3
                0x04 -> parseLacing FixedSizeLacing rest3
                0x06 -> parseLacing EbmlLacing rest3
                _    -> error "Improbable lacing flags"


            tscale           = psTimecodeScale state
            cluster_timecode = psTimecode state
            timecode = cluster_timecode + rel_timecode
            timecodeToSeconds x = (fromIntegral (x*tscale)::Double)*0.000000001
            timecodeSeconds = timecodeToSeconds timecode
            flag_discardable = testBit flags 0
            flag_invisible   = testBit flags 3
            flag_keyframe    = testBit flags 7
            frame = Frame {
                 fTrackNumber = track_number
                ,fTimeCode = timecodeSeconds
                ,fDuration = liftM timecodeToSeconds  dur
                ,fData = contents
                ,fDiscardable = flag_discardable
                ,fKeyframe = flag_keyframe
                ,fInvisible = flag_invisible
                }
        

    handle_timecode :: (MatroskaEvent, ParserState)
    handle_timecode = (MENoop, new_state)
        where
        (MatroskaElement _ _ content) = fromJust $ psElement state
        timecode = case content of
            ECUnsigned x -> x
            _ -> error "Internal error: Timecode element is not unsigned"
        new_state = state{psTimecode = timecode}
        

    

-- The main module function.

parseMkv :: B.ByteString -> [MatroskaEvent]
parseMkv input = events
    where
    initial_state = ParserState { 
         psBuffer         = input
        ,psTimecodeScale  = 1000000
        ,psMode           = ReadEBML
        ,psElement        = Nothing
        ,psTimecode       = 0
        }
    events = unfoldr parseMkv1 initial_state
    
