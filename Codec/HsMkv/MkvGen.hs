module Codec.HsMkv.MkvGen (
     writeMkv
    ,writeMatroskaElement
    ,infoElement
    ,tracksElement
    ,trackElement
    ,frameCluster
    ,rawFrame
    ,matroskaHeader
    ,ebmlHeader
    ,bigEndianNumber
    ,writeEbmlNumber
    )where

import Codec.HsMkv.MkvParse -- Just for data types
import Codec.HsMkv.MkvTabular

import Data.Bits
import Data.Word
import Data.Maybe
import Control.Monad
import Data.Char
import Data.List 
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
    
    
matroskaElement_raw :: EbmlElementID -> Maybe Integer -> B.ByteString -> B.ByteString
matroskaElement_raw id_ size' content_ = B.concat [id_b, size_b, content_]
    where
    id_b = B.pack $ writeEbmlNumber ENUnmodified id_
    size = case size' of
        Just x -> x
        Nothing -> toInteger $ B.length content_
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
-- If size is Nothing it is auto-calculated, Use "-1" for "infinite" (for Segment)
writeMatroskaElement :: MatroskaElement -> B.ByteString
writeMatroskaElement (MatroskaElement kl size content) = 
    matroskaElement_raw (lookupElementIdReverse kl) size (encodeContent content)
    where
    encodeContent (EC_Binary b) = b
    encodeContent (EC_Unsigned b) = B.pack $ bigEndianNumber False  b
    encodeContent (EC_Signed b) = B.pack $ bigEndianNumber True b
    encodeContent (EC_Date b ) = B.pack $ writeEbmlNumber ENUnsigned $ toMatroskaDate b
    encodeContent (EC_TextAscii b) = B.fromChunks [E.encodeUtf8 b]
    encodeContent (EC_TextUtf8 b) = B.fromChunks [E.encodeUtf8 b]
    encodeContent (EC_Float b) =  B.pack $ unsignedBigEndianNumber 8 $ toInteger $ Data.Binary.IEEE754.doubleToWord b
    encodeContent (EC_Master b) = B.concat $ map writeMatroskaElement b

    
    

ebmlHeader :: MatroskaElement
ebmlHeader = MatroskaElement EE_EBML Nothing $ EC_Master [
     MatroskaElement EE_EBMLVersion        Nothing $ EC_Unsigned 1
    ,MatroskaElement EE_EBMLReadVersion    Nothing $ EC_Unsigned 1
    ,MatroskaElement EE_EBMLMaxIDLength    Nothing $ EC_Unsigned 4
    ,MatroskaElement EE_EBMLMaxSizeLength  Nothing $ EC_Unsigned 8
    ,MatroskaElement EE_DocType            Nothing $ EC_TextAscii $ T.pack "matroska"
    ,MatroskaElement EE_DocTypeVersion     Nothing $ EC_Unsigned 2
    ,MatroskaElement EE_DocTypeReadVersion Nothing $ EC_Unsigned 2
    ]


-- Write simple streaming Matroska header
matroskaHeader :: B.ByteString
matroskaHeader = B.concat [
    writeMatroskaElement ebmlHeader,
    writeMatroskaElement $ MatroskaElement EE_Segment (Just (-1)) $ EC_Binary B.empty
    ]


rawFrame :: Integer -> Word8 -> Integer -> [B.ByteString] -> B.ByteString
rawFrame rel_timecode additional_flags track buffers = B.concat [
     B.pack $ writeEbmlNumber ENUnsigned track
    ,B.pack $ unsignedBigEndianNumber 2 rel_timecode'
    ,B.pack [flags]
    ,B.pack lacing
    ,B.concat $ buffers]
    where
    rel_timecode' = case rel_timecode < 0 of
        False -> rel_timecode
        True -> rel_timecode + 0x8000
    flags_xiph = bit 1
    flags = flags_xiph .|. additional_flags
    n = fromIntegral (length buffers ) :: Word8
    (buffersExpectOfLast, _) = splitAt (length buffers - 1) buffers
    lengths = map B.length buffersExpectOfLast
    xiph :: Int -> [Word8]
    xiph x
        | x < 255   = [fromIntegral x]
        | otherwise = 255:(xiph (x-255))
    lacing = (n-1) : (concat $ map (xiph . fromIntegral) lengths)


toMatroskaTimecode :: Integer -> Double -> Integer
toMatroskaTimecode timecode_scale timecode = floor $ (timecode * 1000000000.0) / (fromInteger timecode_scale)

frameCluster :: Integer -> Frame -> MatroskaElement
frameCluster timecode_scale frame =
     MatroskaElement EE_Cluster Nothing $ EC_Master [
        MatroskaElement EE_Timecode Nothing $ EC_Unsigned $ toMatroskaTimecode' (f_timeCode frame)
       ,case f_duration frame of
            Nothing -> MatroskaElement EE_SimpleBlock Nothing $ EC_Binary frameData
            Just duration -> MatroskaElement EE_BlockGroup Nothing $ EC_Master [
                 MatroskaElement EE_BlockDuration Nothing $ EC_Unsigned $ toMatroskaTimecode' duration
                ,MatroskaElement EE_Block Nothing $ EC_Binary frameData 
                ]
    ] where
    toMatroskaTimecode' :: Double -> Integer
    toMatroskaTimecode' = toMatroskaTimecode timecode_scale
    flag_maybe_keyframe    = if f_keyframe    frame then bit 7 else 0
    flag_maybe_discardable = if f_discardable frame then bit 0 else 0
    flag_maybe_invisible   = if f_invisible   frame then bit 3 else 0
    additional_flags = flag_maybe_keyframe .|. flag_maybe_discardable .|. flag_maybe_invisible
    frameData = rawFrame 0 additional_flags (f_trackNumber frame) (f_data frame)
         
    
trackElement :: Track -> MatroskaElement
trackElement track =
    MatroskaElement EE_TrackEntry Nothing (EC_Master $ additional_track_info ++ [
         MatroskaElement EE_TrackNumber Nothing (EC_Unsigned $ t_number track)
        ,MatroskaElement EE_TrackType Nothing (EC_Unsigned $ getTrackType $ t_type track)
        ,MatroskaElement EE_CodecID Nothing (EC_TextAscii $ t_codecId track)
        ])
    where
    getTrackType TT_Video     = 1
    getTrackType TT_Audio     = 2
    getTrackType TT_Complex   = 3
    getTrackType TT_Logo      = 0x10
    getTrackType TT_Subtitle  = 0x11
    getTrackType TT_Button    = 0x12
    getTrackType TT_Control   = 0x20
    getTrackType (TT_Unknown t) = t
    additional_track_info = catMaybes [ Nothing
        ,liftM (\x -> MatroskaElement EE_TrackUID        Nothing $ EC_Unsigned  x) $ t_UID track
        ,liftM (\x -> MatroskaElement EE_MinCache        Nothing $ EC_Unsigned  x) $ t_minCache track
        ,liftM (\x -> MatroskaElement EE_CodecPrivate    Nothing $ EC_Binary    x) $ t_codecPrivate track
        ,liftM (\x -> MatroskaElement EE_DefaultDuration Nothing $ EC_Unsigned $ toMatroskaTimecode' x) $ t_defaultDuration track
        ,liftM (\x -> MatroskaElement EE_Language        Nothing $ EC_TextAscii x) $ t_language track
        ,if (isJust $ t_videoPixelWidth        track) then  Just videoElement else Nothing
        ,if (isJust $ t_audioChannels          track) || 
            (isJust $ t_audioSamplingFrequency track) then  Just audioElement else Nothing
        ]
    videoElement = MatroskaElement EE_Video Nothing $ EC_Master $ catMaybes [ Nothing
        ,liftM (\x -> MatroskaElement EE_PixelWidth      Nothing $ EC_Unsigned  x) $ t_videoPixelWidth track
        ,liftM (\x -> MatroskaElement EE_PixelHeight     Nothing $ EC_Unsigned  x) $ t_videoPixelHeight track
        ,liftM (\x -> MatroskaElement EE_DisplayWidth    Nothing $ EC_Unsigned  x) $ t_videoDisplayWidth track
        ,liftM (\x -> MatroskaElement EE_DisplayHeight   Nothing $ EC_Unsigned  x) $ t_videoDisplayHeight track
        ]
    audioElement = MatroskaElement EE_Audio Nothing $ EC_Master $ catMaybes [ Nothing
        ,liftM (\x -> MatroskaElement EE_SamplingFrequency       Nothing $ EC_Float x) $ t_audioSamplingFrequency track
        ,liftM (\x -> MatroskaElement EE_OutputSamplingFrequency Nothing $ EC_Float x) $ t_audioOutputSamplingFrequency track
        ,liftM (\x -> MatroskaElement EE_Channels                Nothing $ EC_Unsigned x) $ t_audioChannels track
        ]
    toMatroskaTimecode' x = floor (x * 1000000000.0)

tracksElement :: [Track] -> MatroskaElement
tracksElement tracks = MatroskaElement EE_Tracks Nothing $ EC_Master $ map trackElement tracks

infoElement :: Info -> MatroskaElement
infoElement info = MatroskaElement EE_Info Nothing $ EC_Master $ 
    (             MatroskaElement EE_TimecodeScale   Nothing $ EC_Unsigned     $ i_timecodeScale info) :
    catMaybes [ Nothing
    ,liftM (\x -> MatroskaElement EE_MuxingApp       Nothing $ EC_TextUtf8  x) $ i_muxingApplication info
    ,liftM (\x -> MatroskaElement EE_WritingApp      Nothing $ EC_TextUtf8  x) $ i_writingApplication info
    ,liftM (\x -> MatroskaElement EE_Duration        Nothing $ EC_Float     x) $ i_duration info
    ,liftM (\x -> MatroskaElement EE_DateUTC         Nothing $ EC_Date      x) $ i_date info
    ,liftM (\x -> MatroskaElement EE_Title           Nothing $ EC_TextUtf8  x) $ i_title info
    ,liftM (\x -> MatroskaElement EE_SegmentUID      Nothing $ EC_Binary $ fromHex x) $ i_segmentUid info
    ]
    where
    fromHex = B.pack . fromHex' . T.unpack
    fromHex' :: String -> [Word8]
    fromHex' (h:(l:tail)) = b:fromHex' tail
        where 
        b = (bh `shiftL` 4) .|. bl
        bh = nibble h
        bl = nibble l
        nibble x
            | '0' <= x && x <= '9' = fromIntegral $ ord(x) - ord('0')
            | 'A' <= x && x <= 'F' = fromIntegral $ ord(x) - ord('A') + 10
            | 'a' <= x && x <= 'f' = fromIntegral $ ord(x) - ord('a') + 10
    fromHex' [] = []
    fromHex' [x] = error "Odd number of hex digits?"


writeMkv :: [MatroskaEvent] -> B.ByteString
writeMkv events = B.concat (matroskaHeader:unfoldr handle (events,1000000))
    where
    handle :: ([MatroskaEvent], Integer) -> Maybe (B.ByteString, ([MatroskaEvent], Integer))
    handle (((ME_Info info):tail), timescale) = 
        Just (writeMatroskaElement $ infoElement $ tweak_info info, (tail, i_timecodeScale info))
    handle ((ME_Tracks tracks):tail, timescale) = 
        Just (writeMatroskaElement $ tracksElement tracks,          (tail, timescale))
    handle ((ME_Frame  frame ):tail, timescale) = 
        Just (writeMatroskaElement $ frameCluster timescale frame,  (tail, timescale))
    handle ((_:tail), timescale) = 
        Just (B.empty, (tail, timescale))
    handle ([], timescale) = Nothing

    -- Check if "muxingApplication" exists and contains "HsMkv"
    -- add/append "HsMkv" if not
    tweak_info info = new_info
        where
        have_MkvGen_in_muxingApp = case i_muxingApplication info of
            Nothing -> False
            Just x -> case T.count txt_HsMkv x of
                0 -> False
                n -> True
        new_info = if have_MkvGen_in_muxingApp then info else
            case i_muxingApplication info of
                Just x -> info { i_muxingApplication = Just $ T.concat [x, T.pack "; ",txt_HsMkv] }
                Nothing -> info { i_muxingApplication = Just txt_HsMkv }
        txt_HsMkv = T.pack "HsMkv"


        

-- B.writeFile "g.mkv" $ B.concat [matroskaHeader, writeMatroskaElement $ frameCluster 1000000 $ Frame 1 45.4 [B.empty] (Just 2.5)]
