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
unsignedBigEndianNumber 1 x = [fromIntegral x]
unsignedBigEndianNumber n x = head1 : tail1
        where
        n' = n-1
        head1 = fromIntegral $ (x `shiftR` (8*n')) .&. 0xFF 
        tail1 = unsignedBigEndianNumber (n-1) (x .&. complement (0xFF `shiftL` (8*n')))

getEbmlNumberSize :: Integer -> Int
getEbmlNumberSize x
    | x < (2^(7::Int) - 1)   =  1
    | otherwise     = 1 + getEbmlNumberSize ((x+1) `shiftR` 7 - 1)

writeEbmlNumber :: EbmlNumberType -> Integer -> [Word8]
writeEbmlNumber ENUnsigned (-1) =  [255]
writeEbmlNumber ENUnsigned x = head1 : unsignedBigEndianNumber n' rest
    where
    n = getEbmlNumberSize x
    n' = n-1
    first_byte = fromIntegral $ (x `shiftR` (8*n')) .&. 0xFF
    head1 = first_byte .|. (0x80 `shiftR` (n-1))
    rest = x .&. complement (0xFF `shiftL` (8*n'))
writeEbmlNumber ENUnmodified x = head1 : unsignedBigEndianNumber n' rest
    where
    n' = (rank x - 7) `div` 7 
    first_byte = fromIntegral $ (x `shiftR` (8*n')) .&. 0xFF
    head1 = first_byte
    rest = x .&. complement (0xFF `shiftL` (8*n'))
    rank 0 = 0 -- just in case
    rank 1 = 0
    rank v = 1 + rank (v `shiftR` 1)
-- quick fixed-size hack
writeEbmlNumber ENSigned x = 0x08 : unsignedBigEndianNumber additionalSize x'
    where
    x' = x  + (2 ^ (6 + 7 * additionalSize) - 1)
    additionalSize = 4
    
    
matroskaElementRaw :: EbmlElementID -> Maybe Integer -> B.ByteString -> B.ByteString
matroskaElementRaw id_ size' content_ = B.concat [id_b, size_b, content_]
    where
    id_b = B.pack $ writeEbmlNumber ENUnmodified id_
    size = fromMaybe (fromIntegral $ B.length content_) size'
    size_b = B.pack $ writeEbmlNumber ENUnsigned size


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
    x' = if signed && (x < 0)
        then x + (2^(8*niceSize))
        else x


-- Convert MatroskaElement to Bytestring
-- If size is Nothing it is auto-calculated, Use "-1" for "infinite" (for Segment)
writeMatroskaElement :: MatroskaElement -> B.ByteString
writeMatroskaElement (MatroskaElement kl size content) = 
    matroskaElementRaw (lookupElementIdReverse kl) size (encodeContent content)
    where
    encodeContent (ECBinary b) = b
    encodeContent (ECUnsigned b) = B.pack $ bigEndianNumber False  b
    encodeContent (ECSigned b) = B.pack $ bigEndianNumber True b
    encodeContent (ECDate b ) = B.pack $ writeEbmlNumber ENUnsigned $ toMatroskaDate b
    encodeContent (ECTextAscii b) = B.fromChunks [E.encodeUtf8 b]
    encodeContent (ECTextUtf8 b) = B.fromChunks [E.encodeUtf8 b]
    encodeContent (ECFloat b) =  B.pack $ unsignedBigEndianNumber 8 $ fromIntegral $ Data.Binary.IEEE754.doubleToWord b
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
    ,B.concat buffers]
    where
    rel_timecode' = if rel_timecode < 0
        then rel_timecode
        else rel_timecode + 0x8000
    flags_xiph = bit 1
    flags = flags_xiph .|. additional_flags
    n = fromIntegral (length buffers ) :: Word8
    (buffersExpectOfLast, _) = splitAt (length buffers - 1) buffers
    lengths = map B.length buffersExpectOfLast
    xiph :: Int -> [Word8]
    xiph x
        | x < 255   = [fromIntegral x]
        | otherwise = 255 : xiph (x-255)
    lacing = n-1 : concatMap (xiph . fromIntegral) lengths


toMatroskaTimecode :: Integer -> Double -> Integer
toMatroskaTimecode timecode_scale timecode = floor $ (timecode * 1000000000.0) / fromIntegral timecode_scale

frameCluster :: Integer -> Frame -> MatroskaElement
frameCluster timecode_scale frame =
     MatroskaElement EECluster Nothing $ ECMaster [
        MatroskaElement EETimecode Nothing $ ECUnsigned $ toMatroskaTimecode' (fTimeCode frame)
       ,case fDuration frame of
            Nothing -> MatroskaElement EESimpleBlock Nothing $ ECBinary frameData
            Just duration -> MatroskaElement EEBlockGroup Nothing $ ECMaster [
                 MatroskaElement EEBlockDuration Nothing $ ECUnsigned $ toMatroskaTimecode' duration
                ,MatroskaElement EEBlock Nothing $ ECBinary frameData 
                ]
    ] where
    toMatroskaTimecode' :: Double -> Integer
    toMatroskaTimecode' = toMatroskaTimecode timecode_scale
    flag_maybe_keyframe    = if fKeyframe    frame then bit 7 else 0
    flag_maybe_discardable = if fDiscardable frame then bit 0 else 0
    flag_maybe_invisible   = if fInvisible   frame then bit 3 else 0
    additional_flags = flag_maybe_keyframe .|. flag_maybe_discardable .|. flag_maybe_invisible
    frameData = rawFrame 0 additional_flags (fTrackNumber frame) (fData frame)
         
    
trackElement :: Track -> MatroskaElement
trackElement track =
    MatroskaElement EETrackEntry Nothing (ECMaster $ additional_track_info ++ [
         MatroskaElement EETrackNumber Nothing (ECUnsigned $ tNumber track)
        ,MatroskaElement EETrackType Nothing (ECUnsigned $ getTrackType $ tType track)
        ,MatroskaElement EECodecID Nothing (ECTextAscii $ tCodecId track)
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
        ,liftM (MatroskaElement EETrackUID        Nothing . ECUnsigned) $ tUID track
        ,liftM (MatroskaElement EEMinCache        Nothing . ECUnsigned) $ tMinCache track
        ,liftM (MatroskaElement EECodecPrivate    Nothing . ECBinary  ) $ tCodecPrivate track
        ,liftM (MatroskaElement EEDefaultDuration Nothing . ECUnsigned . toMatroskaTimecode') $ tDefaultDuration track
        ,liftM (MatroskaElement EELanguage        Nothing . ECTextAscii) $ tLanguage track
        ,if isJust (tVideoPixelWidth        track) then  Just videoElement else Nothing
        ,if isJust (tAudioChannels          track) || 
            isJust (tAudioSamplingFrequency track) then  Just audioElement else Nothing
        ]
    videoElement = MatroskaElement EEVideo Nothing $ ECMaster $ catMaybes [ Nothing
        ,liftM (MatroskaElement EEPixelWidth      Nothing . ECUnsigned) $ tVideoPixelWidth track
        ,liftM (MatroskaElement EEPixelHeight     Nothing . ECUnsigned) $ tVideoPixelHeight track
        ,liftM (MatroskaElement EEDisplayWidth    Nothing . ECUnsigned) $ tVideoDisplayWidth track
        ,liftM (MatroskaElement EEDisplayHeight   Nothing . ECUnsigned) $ tVideoDisplayHeight track
        ]
    audioElement = MatroskaElement EEAudio Nothing $ ECMaster $ catMaybes [ Nothing
        ,liftM (MatroskaElement EESamplingFrequency       Nothing . ECFloat) $ tAudioSamplingFrequency track
        ,liftM (MatroskaElement EEOutputSamplingFrequency Nothing . ECFloat) $ tAudioOutputSamplingFrequency track
        ,liftM (MatroskaElement EEChannels                Nothing . ECUnsigned) $ tAudioChannels track
        ]
    toMatroskaTimecode' x = floor (x * 1000000000.0)

tracksElement :: [Track] -> MatroskaElement
tracksElement tracks = MatroskaElement EETracks Nothing $ ECMaster $ map trackElement tracks

infoElement :: Info -> MatroskaElement
infoElement info = MatroskaElement EEInfo Nothing $ ECMaster $ 
    MatroskaElement EETimecodeScale   Nothing (ECUnsigned     $ iTimecodeScale info) :
    catMaybes [ Nothing
    ,liftM (MatroskaElement EEMuxingApp       Nothing . ECTextUtf8 ) $ iMuxingApplication info
    ,liftM (MatroskaElement EEWritingApp      Nothing . ECTextUtf8 ) $ iWritingApplication info
    ,liftM (MatroskaElement EEDuration        Nothing . ECFloat    ) $ iDuration info
    ,liftM (MatroskaElement EEDateUTC         Nothing . ECDate     ) $ iDate info
    ,liftM (MatroskaElement EETitle           Nothing . ECTextUtf8 ) $ iTitle info
    ,liftM (MatroskaElement EESegmentUID      Nothing . ECBinary . fromHex) $ iSegmentUid info
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
            | '0' <= x && x <= '9' = fromIntegral $ ord x - ord '0'
            | 'A' <= x && x <= 'F' = fromIntegral $ ord x - ord 'A' + 10
            | 'a' <= x && x <= 'f' = fromIntegral $ ord x - ord 'a' + 10
            | otherwise            = 0x00 -- bad hex nibble
    fromHex' [] = []
    fromHex' [_] = error "Odd number of hex digits?"

-- Check if "muxingApplication" exists and contains "HsMkv"
-- add/append "HsMkv" if not
tweakInfo :: Info -> Info
tweakInfo info = new_info
    where
    have_MkvGen_in_muxingApp = case iMuxingApplication info of
        Nothing -> False
        Just x -> case T.count txt_HsMkv x of
            0 -> False
            _ -> True
    new_info = if have_MkvGen_in_muxingApp then info else
        case iMuxingApplication info of
            Just x -> info { iMuxingApplication = Just $ T.concat [x, T.pack "; ",txt_HsMkv] }
            Nothing -> info { iMuxingApplication = Just txt_HsMkv }
    txt_HsMkv = T.pack "HsMkv"

eventToElement :: Integer -> MatroskaEvent -> Maybe MatroskaElement
eventToElement timescale (MEInfo info) = Just $
    infoElement $ tweakInfo $ info {iTimecodeScale=timescale}
eventToElement _ (METracks tracks) = Just $
    tracksElement tracks
eventToElement timescale (MEFrame frame) = Just $
    frameCluster timescale frame
eventToElement _ _ = Nothing
    

writeMkv :: [MatroskaEvent] -> B.ByteString
writeMkv events = B.concat (matroskaHeader:unfoldr handle (events,1000000))
    where
    handle :: ([MatroskaEvent], Integer) -> Maybe (B.ByteString, ([MatroskaEvent], Integer))
    handle ( MEInfo info   : tail1, _) = 
        Just (writeMatroskaElement $ infoElement $ tweakInfo info, (tail1, iTimecodeScale info))
    handle (METracks tracks: tail1, timescale) = 
        Just (writeMatroskaElement $ tracksElement tracks,          (tail1, timescale))
    handle (MEFrame  frame : tail1, timescale) = 
        Just (writeMatroskaElement $ frameCluster timescale frame,  (tail1, timescale))
    handle (_:tail1, timescale) = 
        Just (B.empty, (tail1, timescale))
    handle ([], _) = Nothing



        

-- B.writeFile "g.mkv" $ B.concat [matroskaHeader, writeMatroskaElement $ frameCluster 1000000 $ Frame 1 45.4 [B.empty] (Just 2.5)]
