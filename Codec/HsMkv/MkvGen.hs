module Codec.HsMkv.MkvGen (
     writeMkv
    ,eventToElement
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

import Codec.HsMkv.Model
import Codec.HsMkv.MkvTabular

import Data.Bits
import Data.Word
import Data.Maybe
import Control.Monad
import Data.Char
import Data.List 
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Data.Binary.IEEE754


unsignedBigEndianNumber :: Int -> Integer -> [Word8]
unsignedBigEndianNumber 0 _ = []
unsignedBigEndianNumber 1 x = [fromInteger x]
unsignedBigEndianNumber n x = head1 : tail1
        where
        n' = n-1
        head1 = fromInteger $ (x `shiftR` (8*n')) .&. 0xFF 
        tail1 = unsignedBigEndianNumber (n-1) (x .&. (complement (0xFF `shiftL` (8*n'))))

getEbmlNumberSize :: Integer -> Int
getEbmlNumberSize x
    | x < (2^(7::Int) - 1)   =  1
    | otherwise     = 1 + getEbmlNumberSize (((x+1) `shiftR` 7 - 1))

writeEbmlNumber :: EbmlNumberType -> Integer -> [Word8]
writeEbmlNumber ENUnsigned (-1) =  [255]
writeEbmlNumber ENUnsigned x = head1 : unsignedBigEndianNumber n' rest
    where
    n = (getEbmlNumberSize x)
    n' = n-1
    first_byte = toInteger $ (x `shiftR` (8*n')) .&. 0xFF
    head1 = fromInteger $ first_byte .|. (0x80 `shiftR` (n-1))
    rest = x .&. (complement (0xFF `shiftL` (8*n')))
writeEbmlNumber ENUnmodified x = head1 : unsignedBigEndianNumber n' rest
    where
    n' = (rank x - 7) `div` 7 
    first_byte = toInteger $ (x `shiftR` (8*n')) .&. 0xFF
    head1 = fromInteger first_byte
    rest = x .&. (complement (0xFF `shiftL` (8*n')))
    rank 0 = 0 -- just in case
    rank 1 = 0
    rank v = 1 + rank (v `shiftR` 1)
-- quick fixed-size hack
writeEbmlNumber ENSigned x = 0x08 : (unsignedBigEndianNumber additionalSize $ x')
    where
    x' = x  + (2 ^ (6 + 7 * additionalSize) - 1)
    additionalSize = 4
    
    
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
    encodeContent (ECBinary b) = b
    encodeContent (ECUnsigned b) = B.pack $ bigEndianNumber False  b
    encodeContent (ECSigned b) = B.pack $ bigEndianNumber True b
    encodeContent (ECDate b ) = B.pack $ writeEbmlNumber ENUnsigned $ toMatroskaDate b
    encodeContent (ECTextAscii b) = B.fromChunks [E.encodeUtf8 b]
    encodeContent (ECTextUtf8 b) = B.fromChunks [E.encodeUtf8 b]
    encodeContent (ECFloat b) =  B.pack $ unsignedBigEndianNumber 8 $ toInteger $ Data.Binary.IEEE754.doubleToWord b
    encodeContent (ECMaster b) = B.concat $ map writeMatroskaElement b

    
    

ebmlHeader :: MatroskaElement
ebmlHeader = MatroskaElement EEEBML Nothing $ ECMaster [
     MatroskaElement EEEBMLVersion        Nothing $ ECUnsigned 1
    ,MatroskaElement EEEBMLReadVersion    Nothing $ ECUnsigned 1
    ,MatroskaElement EEEBMLMaxIDLength    Nothing $ ECUnsigned 4
    ,MatroskaElement EEEBMLMaxSizeLength  Nothing $ ECUnsigned 8
    ,MatroskaElement EEDocType            Nothing $ ECTextAscii $ T.pack "matroska"
    ,MatroskaElement EEDocTypeVersion     Nothing $ ECUnsigned 2
    ,MatroskaElement EEDocTypeReadVersion Nothing $ ECUnsigned 2
    ]


-- Write simple streaming Matroska header
matroskaHeader :: B.ByteString
matroskaHeader = B.concat [
    writeMatroskaElement ebmlHeader,
    writeMatroskaElement $ MatroskaElement EESegment (Just (-1)) $ ECBinary B.empty
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
     MatroskaElement EECluster Nothing $ ECMaster [
        MatroskaElement EETimecode Nothing $ ECUnsigned $ toMatroskaTimecode' (f_timeCode frame)
       ,case f_duration frame of
            Nothing -> MatroskaElement EESimpleBlock Nothing $ ECBinary frameData
            Just duration -> MatroskaElement EEBlockGroup Nothing $ ECMaster [
                 MatroskaElement EEBlockDuration Nothing $ ECUnsigned $ toMatroskaTimecode' duration
                ,MatroskaElement EEBlock Nothing $ ECBinary frameData 
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
    MatroskaElement EETrackEntry Nothing (ECMaster $ additional_track_info ++ [
         MatroskaElement EETrackNumber Nothing (ECUnsigned $ t_number track)
        ,MatroskaElement EETrackType Nothing (ECUnsigned $ getTrackType $ t_type track)
        ,MatroskaElement EECodecID Nothing (ECTextAscii $ t_codecId track)
        ])
    where
    getTrackType TTVideo     = 1
    getTrackType TTAudio     = 2
    getTrackType TTComplex   = 3
    getTrackType TTLogo      = 0x10
    getTrackType TTSubtitle  = 0x11
    getTrackType TTButton    = 0x12
    getTrackType TTControl   = 0x20
    getTrackType (TTUnknown t) = t
    additional_track_info = catMaybes [ Nothing
        ,liftM (\x -> MatroskaElement EETrackUID        Nothing $ ECUnsigned  x) $ t_UID track
        ,liftM (\x -> MatroskaElement EEMinCache        Nothing $ ECUnsigned  x) $ t_minCache track
        ,liftM (\x -> MatroskaElement EECodecPrivate    Nothing $ ECBinary    x) $ t_codecPrivate track
        ,liftM (\x -> MatroskaElement EEDefaultDuration Nothing $ ECUnsigned $ toMatroskaTimecode' x) $ t_defaultDuration track
        ,liftM (\x -> MatroskaElement EELanguage        Nothing $ ECTextAscii x) $ t_language track
        ,if (isJust $ t_videoPixelWidth        track) then  Just videoElement else Nothing
        ,if (isJust $ t_audioChannels          track) || 
            (isJust $ t_audioSamplingFrequency track) then  Just audioElement else Nothing
        ]
    videoElement = MatroskaElement EEVideo Nothing $ ECMaster $ catMaybes [ Nothing
        ,liftM (\x -> MatroskaElement EEPixelWidth      Nothing $ ECUnsigned  x) $ t_videoPixelWidth track
        ,liftM (\x -> MatroskaElement EEPixelHeight     Nothing $ ECUnsigned  x) $ t_videoPixelHeight track
        ,liftM (\x -> MatroskaElement EEDisplayWidth    Nothing $ ECUnsigned  x) $ t_videoDisplayWidth track
        ,liftM (\x -> MatroskaElement EEDisplayHeight   Nothing $ ECUnsigned  x) $ t_videoDisplayHeight track
        ]
    audioElement = MatroskaElement EEAudio Nothing $ ECMaster $ catMaybes [ Nothing
        ,liftM (\x -> MatroskaElement EESamplingFrequency       Nothing $ ECFloat x) $ t_audioSamplingFrequency track
        ,liftM (\x -> MatroskaElement EEOutputSamplingFrequency Nothing $ ECFloat x) $ t_audioOutputSamplingFrequency track
        ,liftM (\x -> MatroskaElement EEChannels                Nothing $ ECUnsigned x) $ t_audioChannels track
        ]
    toMatroskaTimecode' x = floor (x * 1000000000.0)

tracksElement :: [Track] -> MatroskaElement
tracksElement tracks = MatroskaElement EETracks Nothing $ ECMaster $ map trackElement tracks

infoElement :: Info -> MatroskaElement
infoElement info = MatroskaElement EEInfo Nothing $ ECMaster $ 
    (             MatroskaElement EETimecodeScale   Nothing $ ECUnsigned     $ i_timecodeScale info) :
    catMaybes [ Nothing
    ,liftM (\x -> MatroskaElement EEMuxingApp       Nothing $ ECTextUtf8  x) $ i_muxingApplication info
    ,liftM (\x -> MatroskaElement EEWritingApp      Nothing $ ECTextUtf8  x) $ i_writingApplication info
    ,liftM (\x -> MatroskaElement EEDuration        Nothing $ ECFloat     x) $ i_duration info
    ,liftM (\x -> MatroskaElement EEDateUTC         Nothing $ ECDate      x) $ i_date info
    ,liftM (\x -> MatroskaElement EETitle           Nothing $ ECTextUtf8  x) $ i_title info
    ,liftM (\x -> MatroskaElement EESegmentUID      Nothing $ ECBinary $ fromHex x) $ i_segmentUid info
    ]
    where
    fromHex = B.pack . fromHex' . T.unpack
    fromHex' :: String -> [Word8]
    fromHex' (h:(l:tail1)) = b:fromHex' tail1
        where 
        b = (bh `shiftL` 4) .|. bl
        bh = nibble h
        bl = nibble l
        nibble x
            | '0' <= x && x <= '9' = fromIntegral $ ord(x) - ord('0')
            | 'A' <= x && x <= 'F' = fromIntegral $ ord(x) - ord('A') + 10
            | 'a' <= x && x <= 'f' = fromIntegral $ ord(x) - ord('a') + 10
            | otherwise            = 0x00 -- bad hex nibble
    fromHex' [] = []
    fromHex' [_] = error "Odd number of hex digits?"

-- Check if "muxingApplication" exists and contains "HsMkv"
-- add/append "HsMkv" if not
tweak_info :: Info -> Info
tweak_info info = new_info
    where
    have_MkvGen_in_muxingApp = case i_muxingApplication info of
        Nothing -> False
        Just x -> case T.count txt_HsMkv x of
            0 -> False
            _ -> True
    new_info = if have_MkvGen_in_muxingApp then info else
        case i_muxingApplication info of
            Just x -> info { i_muxingApplication = Just $ T.concat [x, T.pack "; ",txt_HsMkv] }
            Nothing -> info { i_muxingApplication = Just txt_HsMkv }
    txt_HsMkv = T.pack "HsMkv"

eventToElement :: Integer -> MatroskaEvent -> Maybe MatroskaElement
eventToElement timescale (MEInfo info) = Just $
    infoElement $ tweak_info $ info {i_timecodeScale=timescale}
eventToElement _ (METracks tracks) = Just $
    tracksElement tracks
eventToElement timescale (MEFrame frame) = Just $
    frameCluster timescale frame
eventToElement _ _ = Nothing
    

writeMkv :: [MatroskaEvent] -> B.ByteString
writeMkv events = B.concat (matroskaHeader:unfoldr handle (events,1000000))
    where
    handle :: ([MatroskaEvent], Integer) -> Maybe (B.ByteString, ([MatroskaEvent], Integer))
    handle (((MEInfo info):tail1), _) = 
        Just (writeMatroskaElement $ infoElement $ tweak_info info, (tail1, i_timecodeScale info))
    handle ((METracks tracks):tail1, timescale) = 
        Just (writeMatroskaElement $ tracksElement tracks,          (tail1, timescale))
    handle ((MEFrame  frame ):tail1, timescale) = 
        Just (writeMatroskaElement $ frameCluster timescale frame,  (tail1, timescale))
    handle ((_:tail1), timescale) = 
        Just (B.empty, (tail1, timescale))
    handle ([], _) = Nothing



        

-- B.writeFile "g.mkv" $ B.concat [matroskaHeader, writeMatroskaElement $ frameCluster 1000000 $ Frame 1 45.4 [B.empty] (Just 2.5)]
