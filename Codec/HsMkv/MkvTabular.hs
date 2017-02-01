module Codec.HsMkv.MkvTabular (
    ElementType(..),
    EbmlElementID,
    ElementClass(..),
    lookupElementId,
    lookupElementIdReverse,
    lookupElementType
    )where

import Data.Int
import Data.Map as Map
import Data.Maybe

type EbmlElementID = Int64

data ElementType =
        ETMaster |
        ETUnsigned |
        ETSigned |
        ETTextAscii |
        ETTextUtf8 |
        ETBinary |
        ETFloat |
        ETDate |
        ETFlatten |
        ETUnknown
        deriving (Show, Eq, Ord, Read)

lookupElementId        :: EbmlElementID -> ElementClass
lookupElementIdReverse :: ElementClass -> EbmlElementID
lookupElementType      :: ElementClass -> ElementType

data ElementClass =
    EEEBML |
    EEEBMLVersion |
    EEEBMLReadVersion |
    EEEBMLMaxIDLength |
    EEEBMLMaxSizeLength |
    EEDocType |
    EEDocTypeVersion |
    EEDocTypeReadVersion |
    EEVoid |
    EECRC32 |
    EESignatureSlot |
    EESignatureAlgo |
    EESignatureHash |
    EESignaturePublicKey |
    EESignature |
    EESignatureElements |
    EESignatureElementList |
    EESignedElement |
    EESegment |
    EESeekHead |
    EESeek |
    EESeekID |
    EESeekPosition |
    EEInfo |
    EESegmentUID |
    EESegmentFilename |
    EEPrevUID |
    EEPrevFilename |
    EENextUID |
    EENextFilename |
    EESegmentFamily |
    EEChapterTranslate |
    EEChapterTranslateEditionUID |
    EEChapterTranslateCodec |
    EEChapterTranslateID |
    EETimecodeScale |
    EEDuration |
    EEDateUTC |
    EETitle |
    EEMuxingApp |
    EEWritingApp |
    EECluster |
    EETimecode |
    EESilentTracks |
    EESilentTrackNumber |
    EEPosition |
    EEPrevSize |
    EESimpleBlock |
    EEBlockGroup |
    EEBlock |
    EEBlockVirtual |
    EEBlockAdditions |
    EEBlockMore |
    EEBlockAddID |
    EEBlockAdditional |
    EEBlockDuration |
    EEReferencePriority |
    EEReferenceBlock |
    EEReferenceVirtual |
    EECodecState |
    EESlices |
    EETimeSlice |
    EELaceNumber |
    EEFrameNumber |
    EEBlockAdditionID |
    EEDelay |
    EESliceDuration |
    EEReferenceFrame |
    EEReferenceOffset |
    EEReferenceTimeCode |
    EEEncryptedBlock |
    EETracks |
    EETrackEntry |
    EETrackNumber |
    EETrackUID |
    EETrackType |
    EEFlagEnabled |
    EEFlagDefault |
    EEFlagForced |
    EEFlagLacing |
    EEMinCache |
    EEMaxCache |
    EEDefaultDuration |
    EETrackTimecodeScale |
    EETrackOffset |
    EEMaxBlockAdditionID |
    EEName |
    EELanguage |
    EECodecID |
    EECodecPrivate |
    EECodecName |
    EEAttachmentLink |
    EECodecSettings |
    EECodecInfoURL |
    EECodecDownloadURL |
    EECodecDecodeAll |
    EETrackOverlay |
    EETrackTranslate |
    EETrackTranslateEditionUID |
    EETrackTranslateCodec |
    EETrackTranslateTrackID |
    EEVideo |
    EEFlagInterlaced |
    EEStereoMode |
    EEOldStereoMode |
    EEPixelWidth |
    EEPixelHeight |
    EEPixelCropBottom |
    EEPixelCropTop |
    EEPixelCropLeft |
    EEPixelCropRight |
    EEDisplayWidth |
    EEDisplayHeight |
    EEDisplayUnit |
    EEAspectRatioType |
    EEColourSpace |
    EEGammaValue |
    EEFrameRate |
    EEAudio |
    EESamplingFrequency |
    EEOutputSamplingFrequency |
    EEChannels |
    EEChannelPositions |
    EEBitDepth |
    EETrackOperation |
    EETrackCombinePlanes |
    EETrackPlane |
    EETrackPlaneUID |
    EETrackPlaneType |
    EETrackJoinBlocks |
    EETrackJoinUID |
    EETrickTrackUID |
    EETrickTrackSegmentUID |
    EETrickTrackFlag |
    EETrickMasterTrackUID |
    EETrickMasterTrackSegmentUID |
    EEContentEncodings |
    EEContentEncoding |
    EEContentEncodingOrder |
    EEContentEncodingScope |
    EEContentEncodingType |
    EEContentCompression |
    EEContentCompAlgo |
    EEContentCompSettings |
    EEContentEncryption |
    EEContentEncAlgo |
    EEContentEncKeyID |
    EEContentSignature |
    EEContentSigKeyID |
    EEContentSigAlgo |
    EEContentSigHashAlgo |
    EECues |
    EECuePoint |
    EECueTime |
    EECueTrackPositions |
    EECueTrack |
    EECueClusterPosition |
    EECueBlockNumber |
    EECueCodecState |
    EECueReference |
    EECueRefTime |
    EECueRefCluster |
    EECueRefNumber |
    EECueRefCodecState |
    EEAttachments |
    EEAttachedFile |
    EEFileDescription |
    EEFileName |
    EEFileMimeType |
    EEFileData |
    EEFileUID |
    EEFileReferral |
    EEFileUsedStartTime |
    EEFileUsedEndTime |
    EEChapters |
    EEEditionEntry |
    EEEditionUID |
    EEEditionFlagHidden |
    EEEditionFlagDefault |
    EEEditionFlagOrdered |
    EEChapterAtom |
    EEChapterUID |
    EEChapterTimeStart |
    EEChapterTimeEnd |
    EEChapterFlagHidden |
    EEChapterFlagEnabled |
    EEChapterSegmentUID |
    EEChapterSegmentEditionUID |
    EEChapterPhysicalEquiv |
    EEChapterTrack |
    EEChapterTrackNumber |
    EEChapterDisplay |
    EEChapString |
    EEChapLanguage |
    EEChapCountry |
    EEChapProcess |
    EEChapProcessCodecID |
    EEChapProcessPrivate |
    EEChapProcessCommand |
    EEChapProcessTime |
    EEChapProcessData |
    EETags |
    EETag |
    EETargets |
    EETargetTypeValue |
    EETargetType |
    EETagTrackUID |
    EETagEditionUID |
    EETagChapterUID |
    EETagAttachmentUID |
    EESimpleTag |
    EETagName |
    EETagLanguage |
    EETagDefault |
    EETagString |
    EETagBinary |
    EEAlphaMode |
    EEBitsPerChannel |
    EECbSubsamplingHorz |
    EECbSubsamplingVert |
    EEChapterStringUID |
    EEChromaSitingHorz |
    EEChromaSitingVert |
    EEChromaSubsamplingHorz |
    EEChromaSubsamplingVert |
    EECodecDelay |
    EEColour |
    EEDefaultDecodedFieldDuration |
    EEDiscardPadding |
    EEFieldOrder |
    EELuminanceMax |
    EELuminanceMin |
    EEMasteringMetadata |
    EEMatrixCoefficients |
    EEMaxCLL |
    EEMaxFALL |
    EEPrimaries |
    EEPrimaryBChromaticityX |
    EEPrimaryBChromaticityY |
    EEPrimaryGChromaticityX |
    EEPrimaryGChromaticityY |
    EEPrimaryRChromaticityX |
    EEPrimaryRChromaticityY |
    EERange |
    EESeekPreRoll |
    EETransferCharacteristics |
    EEWhitePointChromaticityX |
    EEWhitePointChromaticityY |
    EEUnknown EbmlElementID
    deriving (Show, Eq, Ord, Read)

lookupElementIdReverse (EEUnknown id_) = id_
lookupElementIdReverse kl = fromJust $ Map.lookup kl mapClassToId

lookupElementId id_ = fromMaybe (EEUnknown id_) $ Map.lookup id_ mapIdToClass

lookupElementType kl = fromMaybe ETUnknown $ Map.lookup kl mapClassToType

mapIdToClass :: Map EbmlElementID ElementClass
mapIdToClass   = Map.fromList $ Prelude.map (\(kl, id_, _) -> (id_, kl)) elementInformation

mapClassToId :: Map ElementClass EbmlElementID
mapClassToId   = Map.fromList $ Prelude.map (\(kl, id_, _) -> (kl, id_)) elementInformation

mapClassToType :: Map ElementClass ElementType
mapClassToType = Map.fromList $ Prelude.map (\(kl, _, type_) -> (kl, type_)) elementInformation

elementInformation :: [(ElementClass, EbmlElementID, ElementType)]
elementInformation = [
     (EEEBML                        , 0x1A45DFA3, ETMaster       )
    ,(EEEBMLVersion                 , 0x00004286, ETUnsigned     )
    ,(EEEBMLReadVersion             , 0x000042F7, ETUnsigned     )
    ,(EEEBMLMaxIDLength             , 0x000042F2, ETUnsigned     )
    ,(EEEBMLMaxSizeLength           , 0x000042F3, ETUnsigned     )
    ,(EEDocType                     , 0x00004282, ETTextAscii    )
    ,(EEDocTypeVersion              , 0x00004287, ETUnsigned     )
    ,(EEDocTypeReadVersion          , 0x00004285, ETUnsigned     )
    ,(EEVoid                        , 0x000000EC, ETBinary       )
    ,(EECRC32                       , 0x000000BF, ETBinary       )
    ,(EESignatureSlot               , 0x1B538667, ETMaster       )
    ,(EESignatureAlgo               , 0x00007E8A, ETUnsigned     )
    ,(EESignatureHash               , 0x00007E9A, ETUnsigned     )
    ,(EESignaturePublicKey          , 0x00007EA5, ETBinary       )
    ,(EESignature                   , 0x00007EB5, ETBinary       )
    ,(EESignatureElements           , 0x00007E5B, ETMaster       )
    ,(EESignatureElementList        , 0x00007E7B, ETMaster       )
    ,(EESignedElement               , 0x00006532, ETBinary       )
    ,(EESegment                     , 0x18538067, ETFlatten      )
    ,(EESeekHead                    , 0x114D9B74, ETMaster       )
    ,(EESeek                        , 0x00004DBB, ETMaster       )
    ,(EESeekID                      , 0x000053AB, ETBinary       )
    ,(EESeekPosition                , 0x000053AC, ETUnsigned     )
    ,(EEInfo                        , 0x1549A966, ETMaster       )
    ,(EESegmentUID                  , 0x000073A4, ETBinary       )
    ,(EESegmentFilename             , 0x00007384, ETTextUtf8     )
    ,(EEPrevUID                     , 0x003CB923, ETBinary       )
    ,(EEPrevFilename                , 0x003C83AB, ETTextUtf8     )
    ,(EENextUID                     , 0x003EB923, ETBinary       )
    ,(EENextFilename                , 0x003E83BB, ETTextUtf8     )
    ,(EESegmentFamily               , 0x00004444, ETBinary       )
    ,(EEChapterTranslate            , 0x00006924, ETMaster       )
    ,(EEChapterTranslateEditionUID  , 0x000069FC, ETUnsigned     )
    ,(EEChapterTranslateCodec       , 0x000069BF, ETUnsigned     )
    ,(EEChapterTranslateID          , 0x000069A5, ETBinary       )
    ,(EETimecodeScale               , 0x002AD7B1, ETUnsigned     )
    ,(EEDuration                    , 0x00004489, ETFloat        )
    ,(EEDateUTC                     , 0x00004461, ETDate         )
    ,(EETitle                       , 0x00007BA9, ETTextUtf8     )
    ,(EEMuxingApp                   , 0x00004D80, ETTextUtf8     )
    ,(EEWritingApp                  , 0x00005741, ETTextUtf8     )
    ,(EECluster                     , 0x1F43B675, ETFlatten      )
    ,(EETimecode                    , 0x000000E7, ETUnsigned     )
    ,(EESilentTracks                , 0x00005854, ETMaster       )
    ,(EESilentTrackNumber           , 0x000058D7, ETUnsigned     )
    ,(EEPosition                    , 0x000000A7, ETUnsigned     )
    ,(EEPrevSize                    , 0x000000AB, ETUnsigned     )
    ,(EESimpleBlock                 , 0x000000A3, ETBinary       )
    ,(EEBlockGroup                  , 0x000000A0, ETMaster       )
    ,(EEBlock                       , 0x000000A1, ETBinary       )
    ,(EEBlockVirtual                , 0x000000A2, ETBinary       )
    ,(EEBlockAdditions              , 0x000075A1, ETMaster       )
    ,(EEBlockMore                   , 0x000000A6, ETMaster       )
    ,(EEBlockAddID                  , 0x000000EE, ETUnsigned     )
    ,(EEBlockAdditional             , 0x000000A5, ETBinary       )
    ,(EEBlockDuration               , 0x0000009B, ETUnsigned     )
    ,(EEReferencePriority           , 0x000000FA, ETUnsigned     )
    ,(EEReferenceBlock              , 0x000000FB, ETSigned       )
    ,(EEReferenceVirtual            , 0x000000FD, ETSigned       )
    ,(EECodecState                  , 0x000000A4, ETBinary       )
    ,(EESlices                      , 0x0000008E, ETMaster       )
    ,(EETimeSlice                   , 0x000000E8, ETMaster       )
    ,(EELaceNumber                  , 0x000000CC, ETUnsigned     )
    ,(EEFrameNumber                 , 0x000000CD, ETUnsigned     )
    ,(EEBlockAdditionID             , 0x000000CB, ETUnsigned     )
    ,(EEDelay                       , 0x000000CE, ETUnsigned     )
    ,(EESliceDuration               , 0x000000CF, ETUnsigned     )
    ,(EEReferenceFrame              , 0x000000C8, ETMaster       )
    ,(EEReferenceOffset             , 0x000000C9, ETUnsigned     )
    ,(EEReferenceTimeCode           , 0x000000CA, ETUnsigned     )
    ,(EEEncryptedBlock              , 0x000000AF, ETBinary       )
    ,(EETracks                      , 0x1654AE6B, ETMaster       )
    ,(EETrackEntry                  , 0x000000AE, ETMaster       )
    ,(EETrackNumber                 , 0x000000D7, ETUnsigned     )
    ,(EETrackUID                    , 0x000073C5, ETUnsigned     )
    ,(EETrackType                   , 0x00000083, ETUnsigned     )
    ,(EEFlagEnabled                 , 0x000000B9, ETUnsigned     )
    ,(EEFlagDefault                 , 0x00000088, ETUnsigned     )
    ,(EEFlagForced                  , 0x000055AA, ETUnsigned     )
    ,(EEFlagLacing                  , 0x0000009C, ETUnsigned     )
    ,(EEMinCache                    , 0x00006DE7, ETUnsigned     )
    ,(EEMaxCache                    , 0x00006DF8, ETUnsigned     )
    ,(EEDefaultDuration             , 0x0023E383, ETUnsigned     )
    ,(EETrackTimecodeScale          , 0x0023314F, ETFloat        )
    ,(EETrackOffset                 , 0x0000537F, ETSigned       )
    ,(EEMaxBlockAdditionID          , 0x000055EE, ETUnsigned     )
    ,(EEName                        , 0x0000536E, ETTextUtf8     )
    ,(EELanguage                    , 0x0022B59C, ETTextAscii    )
    ,(EECodecID                     , 0x00000086, ETTextAscii    )
    ,(EECodecPrivate                , 0x000063A2, ETBinary       )
    ,(EECodecName                   , 0x00258688, ETTextUtf8     )
    ,(EEAttachmentLink              , 0x00007446, ETUnsigned     )
    ,(EECodecSettings               , 0x003A9697, ETTextUtf8     )
    ,(EECodecInfoURL                , 0x003B4040, ETTextAscii    )
    ,(EECodecDownloadURL            , 0x0026B240, ETTextAscii    )
    ,(EECodecDecodeAll              , 0x000000AA, ETUnsigned     )
    ,(EETrackOverlay                , 0x00006FAB, ETUnsigned     )
    ,(EETrackTranslate              , 0x00006624, ETMaster       )
    ,(EETrackTranslateEditionUID    , 0x000066FC, ETUnsigned     )
    ,(EETrackTranslateCodec         , 0x000066BF, ETUnsigned     )
    ,(EETrackTranslateTrackID       , 0x000066A5, ETBinary       )
    ,(EEVideo                       , 0x000000E0, ETMaster       )
    ,(EEFlagInterlaced              , 0x0000009A, ETUnsigned     )
    ,(EEStereoMode                  , 0x000053B8, ETUnsigned     )
    ,(EEOldStereoMode               , 0x000053B9, ETUnsigned     )
    ,(EEPixelWidth                  , 0x000000B0, ETUnsigned     )
    ,(EEPixelHeight                 , 0x000000BA, ETUnsigned     )
    ,(EEPixelCropBottom             , 0x000054AA, ETUnsigned     )
    ,(EEPixelCropTop                , 0x000054BB, ETUnsigned     )
    ,(EEPixelCropLeft               , 0x000054CC, ETUnsigned     )
    ,(EEPixelCropRight              , 0x000054DD, ETUnsigned     )
    ,(EEDisplayWidth                , 0x000054B0, ETUnsigned     )
    ,(EEDisplayHeight               , 0x000054BA, ETUnsigned     )
    ,(EEDisplayUnit                 , 0x000054B2, ETUnsigned     )
    ,(EEAspectRatioType             , 0x000054B3, ETUnsigned     )
    ,(EEColourSpace                 , 0x002EB524, ETBinary       )
    ,(EEGammaValue                  , 0x002FB523, ETFloat        )
    ,(EEFrameRate                   , 0x002383E3, ETFloat        )
    ,(EEAudio                       , 0x000000E1, ETMaster       )
    ,(EESamplingFrequency           , 0x000000B5, ETFloat        )
    ,(EEOutputSamplingFrequency     , 0x000078B5, ETFloat        )
    ,(EEChannels                    , 0x0000009F, ETUnsigned     )
    ,(EEChannelPositions            , 0x00007D7B, ETBinary       )
    ,(EEBitDepth                    , 0x00006264, ETUnsigned     )
    ,(EETrackOperation              , 0x000000E2, ETMaster       )
    ,(EETrackCombinePlanes          , 0x000000E3, ETMaster       )
    ,(EETrackPlane                  , 0x000000E4, ETMaster       )
    ,(EETrackPlaneUID               , 0x000000E5, ETUnsigned     )
    ,(EETrackPlaneType              , 0x000000E6, ETUnsigned     )
    ,(EETrackJoinBlocks             , 0x000000E9, ETMaster       )
    ,(EETrackJoinUID                , 0x000000ED, ETUnsigned     )
    ,(EETrickTrackUID               , 0x000000C0, ETUnsigned     )
    ,(EETrickTrackSegmentUID        , 0x000000C1, ETBinary       )
    ,(EETrickTrackFlag              , 0x000000C6, ETUnsigned     )
    ,(EETrickMasterTrackUID         , 0x000000C7, ETUnsigned     )
    ,(EETrickMasterTrackSegmentUID  , 0x000000C4, ETBinary       )
    ,(EEContentEncodings            , 0x00006D80, ETMaster       )
    ,(EEContentEncoding             , 0x00006240, ETMaster       )
    ,(EEContentEncodingOrder        , 0x00005031, ETUnsigned     )
    ,(EEContentEncodingScope        , 0x00005032, ETUnsigned     )
    ,(EEContentEncodingType         , 0x00005033, ETUnsigned     )
    ,(EEContentCompression          , 0x00005034, ETMaster       )
    ,(EEContentCompAlgo             , 0x00004254, ETUnsigned     )
    ,(EEContentCompSettings         , 0x00004255, ETBinary       )
    ,(EEContentEncryption           , 0x00005035, ETMaster       )
    ,(EEContentEncAlgo              , 0x000047E1, ETUnsigned     )
    ,(EEContentEncKeyID             , 0x000047E2, ETBinary       )
    ,(EEContentSignature            , 0x000047E3, ETBinary       )
    ,(EEContentSigKeyID             , 0x000047E4, ETBinary       )
    ,(EEContentSigAlgo              , 0x000047E5, ETUnsigned     )
    ,(EEContentSigHashAlgo          , 0x000047E6, ETUnsigned     )
    ,(EECues                        , 0x1C53BB6B, ETMaster       )
    ,(EECuePoint                    , 0x000000BB, ETMaster       )
    ,(EECueTime                     , 0x000000B3, ETUnsigned     )
    ,(EECueTrackPositions           , 0x000000B7, ETMaster       )
    ,(EECueTrack                    , 0x000000F7, ETUnsigned     )
    ,(EECueClusterPosition          , 0x000000F1, ETUnsigned     )
    ,(EECueBlockNumber              , 0x00005378, ETUnsigned     )
    ,(EECueCodecState               , 0x000000EA, ETUnsigned     )
    ,(EECueReference                , 0x000000DB, ETMaster       )
    ,(EECueRefTime                  , 0x00000096, ETUnsigned     )
    ,(EECueRefCluster               , 0x00000097, ETUnsigned     )
    ,(EECueRefNumber                , 0x0000535F, ETUnsigned     )
    ,(EECueRefCodecState            , 0x000000EB, ETUnsigned     )
    ,(EEAttachments                 , 0x1941A469, ETMaster       )
    ,(EEAttachedFile                , 0x000061A7, ETMaster       )
    ,(EEFileDescription             , 0x0000467E, ETTextUtf8     )
    ,(EEFileName                    , 0x0000466E, ETTextUtf8     )
    ,(EEFileMimeType                , 0x00004660, ETTextAscii    )
    ,(EEFileData                    , 0x0000465C, ETBinary       )
    ,(EEFileUID                     , 0x000046AE, ETUnsigned     )
    ,(EEFileReferral                , 0x00004675, ETBinary       )
    ,(EEFileUsedStartTime           , 0x00004661, ETUnsigned     )
    ,(EEFileUsedEndTime             , 0x00004662, ETUnsigned     )
    ,(EEChapters                    , 0x1043A770, ETMaster       )
    ,(EEEditionEntry                , 0x000045B9, ETMaster       )
    ,(EEEditionUID                  , 0x000045BC, ETUnsigned     )
    ,(EEEditionFlagHidden           , 0x000045BD, ETUnsigned     )
    ,(EEEditionFlagDefault          , 0x000045DB, ETUnsigned     )
    ,(EEEditionFlagOrdered          , 0x000045DD, ETUnsigned     )
    ,(EEChapterAtom                 , 0x000000B6, ETMaster       )
    ,(EEChapterUID                  , 0x000073C4, ETUnsigned     )
    ,(EEChapterTimeStart            , 0x00000091, ETUnsigned     )
    ,(EEChapterTimeEnd              , 0x00000092, ETUnsigned     )
    ,(EEChapterFlagHidden           , 0x00000098, ETUnsigned     )
    ,(EEChapterFlagEnabled          , 0x00004598, ETUnsigned     )
    ,(EEChapterSegmentUID           , 0x00006E67, ETBinary       )
    ,(EEChapterSegmentEditionUID    , 0x00006EBC, ETUnsigned     )
    ,(EEChapterPhysicalEquiv        , 0x000063C3, ETUnsigned     )
    ,(EEChapterTrack                , 0x0000008F, ETMaster       )
    ,(EEChapterTrackNumber          , 0x00000089, ETUnsigned     )
    ,(EEChapterDisplay              , 0x00000080, ETMaster       )
    ,(EEChapString                  , 0x00000085, ETTextUtf8     )
    ,(EEChapLanguage                , 0x0000437C, ETTextAscii    )
    ,(EEChapCountry                 , 0x0000437E, ETTextAscii    )
    ,(EEChapProcess                 , 0x00006944, ETMaster       )
    ,(EEChapProcessCodecID          , 0x00006955, ETUnsigned     )
    ,(EEChapProcessPrivate          , 0x0000450D, ETBinary       )
    ,(EEChapProcessCommand          , 0x00006911, ETMaster       )
    ,(EEChapProcessTime             , 0x00006922, ETUnsigned     )
    ,(EEChapProcessData             , 0x00006933, ETBinary       )
    ,(EETags                        , 0x1254C367, ETMaster       )
    ,(EETag                         , 0x00007373, ETMaster       )
    ,(EETargets                     , 0x000063C0, ETMaster       )
    ,(EETargetTypeValue             , 0x000068CA, ETUnsigned     )
    ,(EETargetType                  , 0x000063CA, ETTextAscii    )
    ,(EETagTrackUID                 , 0x000063C5, ETUnsigned     )
    ,(EETagEditionUID               , 0x000063C9, ETUnsigned     )
    ,(EETagChapterUID               , 0x000063C4, ETUnsigned     )
    ,(EETagAttachmentUID            , 0x000063C6, ETUnsigned     )
    ,(EESimpleTag                   , 0x000067C8, ETMaster       )
    ,(EETagName                     , 0x000045A3, ETTextUtf8     )
    ,(EETagLanguage                 , 0x0000447A, ETTextAscii    )
    ,(EETagDefault                  , 0x00004484, ETUnsigned     )
    ,(EETagString                   , 0x00004487, ETTextUtf8     )
    ,(EETagBinary                   , 0x00004485, ETBinary       )
    ,(EEAlphaMode                   , 0x000053C0, ETUnsigned     )
    ,(EEBitsPerChannel              , 0x000055B2, ETUnsigned     )
    ,(EECbSubsamplingHorz           , 0x000055B5, ETUnsigned     )
    ,(EECbSubsamplingVert           , 0x000055B6, ETUnsigned     )
    ,(EEChapterStringUID            , 0x00005654, ETTextUtf8     )
    ,(EEChromaSitingHorz            , 0x000055B7, ETUnsigned     )
    ,(EEChromaSitingVert            , 0x000055B8, ETUnsigned     )
    ,(EEChromaSubsamplingHorz       , 0x000055B3, ETUnsigned     )
    ,(EEChromaSubsamplingVert       , 0x000055B4, ETUnsigned     )
    ,(EECodecDelay                  , 0x000056AA, ETUnsigned     )
    ,(EEColour                      , 0x000055B0, ETMaster       )
    ,(EEDefaultDecodedFieldDuration , 0x00234E7A, ETUnsigned     )
    ,(EEDiscardPadding              , 0x000075A2, ETSigned       )
    ,(EEFieldOrder                  , 0x0000009D, ETUnsigned     )
    ,(EELuminanceMax                , 0x000055D9, ETFloat        )
    ,(EELuminanceMin                , 0x000055DA, ETFloat        )
    ,(EEMasteringMetadata           , 0x000055D0, ETMaster       )
    ,(EEMatrixCoefficients          , 0x000055B1, ETUnsigned     )
    ,(EEMaxCLL                      , 0x000055BC, ETUnsigned     )
    ,(EEMaxFALL                     , 0x000055BD, ETUnsigned     )
    ,(EEPrimaries                   , 0x000055BB, ETUnsigned     )
    ,(EEPrimaryBChromaticityX       , 0x000055D5, ETFloat        )
    ,(EEPrimaryBChromaticityY       , 0x000055D6, ETFloat        )
    ,(EEPrimaryGChromaticityX       , 0x000055D3, ETFloat        )
    ,(EEPrimaryGChromaticityY       , 0x000055D4, ETFloat        )
    ,(EEPrimaryRChromaticityX       , 0x000055D1, ETFloat        )
    ,(EEPrimaryRChromaticityY       , 0x000055D2, ETFloat        )
    ,(EERange                       , 0x000055B9, ETUnsigned     )
    ,(EESeekPreRoll                 , 0x000056BB, ETUnsigned     )
    ,(EETransferCharacteristics     , 0x000055BA, ETUnsigned     )
    ,(EEWhitePointChromaticityX     , 0x000055D7, ETFloat        )
    ,(EEWhitePointChromaticityY     , 0x000055D8, ETFloat        )
    ]
