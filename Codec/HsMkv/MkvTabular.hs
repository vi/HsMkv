module Codec.HsMkv.MkvTabular (
    ElementType(..),
    EbmlElementID,
    ElementClass(..),
    lookupElementId,
    lookupElementIdReverse,
    lookupElementType
    )where

type EbmlElementID = Integer

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
    EEUnknown EbmlElementID
    deriving (Show, Eq, Ord, Read)

lookupElementIdReverse EEEBML = 0x1A45DFA3
lookupElementIdReverse EEEBMLVersion = 0x00004286
lookupElementIdReverse EEEBMLReadVersion = 0x000042F7
lookupElementIdReverse EEEBMLMaxIDLength = 0x000042F2
lookupElementIdReverse EEEBMLMaxSizeLength = 0x000042F3
lookupElementIdReverse EEDocType = 0x00004282
lookupElementIdReverse EEDocTypeVersion = 0x00004287
lookupElementIdReverse EEDocTypeReadVersion = 0x00004285
lookupElementIdReverse EEVoid = 0x000000EC
lookupElementIdReverse EECRC32 = 0x000000BF
lookupElementIdReverse EESignatureSlot = 0x1B538667
lookupElementIdReverse EESignatureAlgo = 0x00007E8A
lookupElementIdReverse EESignatureHash = 0x00007E9A
lookupElementIdReverse EESignaturePublicKey = 0x00007EA5
lookupElementIdReverse EESignature = 0x00007EB5
lookupElementIdReverse EESignatureElements = 0x00007E5B
lookupElementIdReverse EESignatureElementList = 0x00007E7B
lookupElementIdReverse EESignedElement = 0x00006532
lookupElementIdReverse EESegment = 0x18538067
lookupElementIdReverse EESeekHead = 0x114D9B74
lookupElementIdReverse EESeek = 0x00004DBB
lookupElementIdReverse EESeekID = 0x000053AB
lookupElementIdReverse EESeekPosition = 0x000053AC
lookupElementIdReverse EEInfo = 0x1549A966
lookupElementIdReverse EESegmentUID = 0x000073A4
lookupElementIdReverse EESegmentFilename = 0x00007384
lookupElementIdReverse EEPrevUID = 0x003CB923
lookupElementIdReverse EEPrevFilename = 0x003C83AB
lookupElementIdReverse EENextUID = 0x003EB923
lookupElementIdReverse EENextFilename = 0x003E83BB
lookupElementIdReverse EESegmentFamily = 0x00004444
lookupElementIdReverse EEChapterTranslate = 0x00006924
lookupElementIdReverse EEChapterTranslateEditionUID = 0x000069FC
lookupElementIdReverse EEChapterTranslateCodec = 0x000069BF
lookupElementIdReverse EEChapterTranslateID = 0x000069A5
lookupElementIdReverse EETimecodeScale = 0x002AD7B1
lookupElementIdReverse EEDuration = 0x00004489
lookupElementIdReverse EEDateUTC = 0x00004461
lookupElementIdReverse EETitle = 0x00007BA9
lookupElementIdReverse EEMuxingApp = 0x00004D80
lookupElementIdReverse EEWritingApp = 0x00005741
lookupElementIdReverse EECluster = 0x1F43B675
lookupElementIdReverse EETimecode = 0x000000E7
lookupElementIdReverse EESilentTracks = 0x00005854
lookupElementIdReverse EESilentTrackNumber = 0x000058D7
lookupElementIdReverse EEPosition = 0x000000A7
lookupElementIdReverse EEPrevSize = 0x000000AB
lookupElementIdReverse EESimpleBlock = 0x000000A3
lookupElementIdReverse EEBlockGroup = 0x000000A0
lookupElementIdReverse EEBlock = 0x000000A1
lookupElementIdReverse EEBlockVirtual = 0x000000A2
lookupElementIdReverse EEBlockAdditions = 0x000075A1
lookupElementIdReverse EEBlockMore = 0x000000A6
lookupElementIdReverse EEBlockAddID = 0x000000EE
lookupElementIdReverse EEBlockAdditional = 0x000000A5
lookupElementIdReverse EEBlockDuration = 0x0000009B
lookupElementIdReverse EEReferencePriority = 0x000000FA
lookupElementIdReverse EEReferenceBlock = 0x000000FB
lookupElementIdReverse EEReferenceVirtual = 0x000000FD
lookupElementIdReverse EECodecState = 0x000000A4
lookupElementIdReverse EESlices = 0x0000008E
lookupElementIdReverse EETimeSlice = 0x000000E8
lookupElementIdReverse EELaceNumber = 0x000000CC
lookupElementIdReverse EEFrameNumber = 0x000000CD
lookupElementIdReverse EEBlockAdditionID = 0x000000CB
lookupElementIdReverse EEDelay = 0x000000CE
lookupElementIdReverse EESliceDuration = 0x000000CF
lookupElementIdReverse EEReferenceFrame = 0x000000C8
lookupElementIdReverse EEReferenceOffset = 0x000000C9
lookupElementIdReverse EEReferenceTimeCode = 0x000000CA
lookupElementIdReverse EEEncryptedBlock = 0x000000AF
lookupElementIdReverse EETracks = 0x1654AE6B
lookupElementIdReverse EETrackEntry = 0x000000AE
lookupElementIdReverse EETrackNumber = 0x000000D7
lookupElementIdReverse EETrackUID = 0x000073C5
lookupElementIdReverse EETrackType = 0x00000083
lookupElementIdReverse EEFlagEnabled = 0x000000B9
lookupElementIdReverse EEFlagDefault = 0x00000088
lookupElementIdReverse EEFlagForced = 0x000055AA
lookupElementIdReverse EEFlagLacing = 0x0000009C
lookupElementIdReverse EEMinCache = 0x00006DE7
lookupElementIdReverse EEMaxCache = 0x00006DF8
lookupElementIdReverse EEDefaultDuration = 0x0023E383
lookupElementIdReverse EETrackTimecodeScale = 0x0023314F
lookupElementIdReverse EETrackOffset = 0x0000537F
lookupElementIdReverse EEMaxBlockAdditionID = 0x000055EE
lookupElementIdReverse EEName = 0x0000536E
lookupElementIdReverse EELanguage = 0x0022B59C
lookupElementIdReverse EECodecID = 0x00000086
lookupElementIdReverse EECodecPrivate = 0x000063A2
lookupElementIdReverse EECodecName = 0x00258688
lookupElementIdReverse EEAttachmentLink = 0x00007446
lookupElementIdReverse EECodecSettings = 0x003A9697
lookupElementIdReverse EECodecInfoURL = 0x003B4040
lookupElementIdReverse EECodecDownloadURL = 0x0026B240
lookupElementIdReverse EECodecDecodeAll = 0x000000AA
lookupElementIdReverse EETrackOverlay = 0x00006FAB
lookupElementIdReverse EETrackTranslate = 0x00006624
lookupElementIdReverse EETrackTranslateEditionUID = 0x000066FC
lookupElementIdReverse EETrackTranslateCodec = 0x000066BF
lookupElementIdReverse EETrackTranslateTrackID = 0x000066A5
lookupElementIdReverse EEVideo = 0x000000E0
lookupElementIdReverse EEFlagInterlaced = 0x0000009A
lookupElementIdReverse EEStereoMode = 0x000053B8
lookupElementIdReverse EEOldStereoMode = 0x000053B9
lookupElementIdReverse EEPixelWidth = 0x000000B0
lookupElementIdReverse EEPixelHeight = 0x000000BA
lookupElementIdReverse EEPixelCropBottom = 0x000054AA
lookupElementIdReverse EEPixelCropTop = 0x000054BB
lookupElementIdReverse EEPixelCropLeft = 0x000054CC
lookupElementIdReverse EEPixelCropRight = 0x000054DD
lookupElementIdReverse EEDisplayWidth = 0x000054B0
lookupElementIdReverse EEDisplayHeight = 0x000054BA
lookupElementIdReverse EEDisplayUnit = 0x000054B2
lookupElementIdReverse EEAspectRatioType = 0x000054B3
lookupElementIdReverse EEColourSpace = 0x002EB524
lookupElementIdReverse EEGammaValue = 0x002FB523
lookupElementIdReverse EEFrameRate = 0x002383E3
lookupElementIdReverse EEAudio = 0x000000E1
lookupElementIdReverse EESamplingFrequency = 0x000000B5
lookupElementIdReverse EEOutputSamplingFrequency = 0x000078B5
lookupElementIdReverse EEChannels = 0x0000009F
lookupElementIdReverse EEChannelPositions = 0x00007D7B
lookupElementIdReverse EEBitDepth = 0x00006264
lookupElementIdReverse EETrackOperation = 0x000000E2
lookupElementIdReverse EETrackCombinePlanes = 0x000000E3
lookupElementIdReverse EETrackPlane = 0x000000E4
lookupElementIdReverse EETrackPlaneUID = 0x000000E5
lookupElementIdReverse EETrackPlaneType = 0x000000E6
lookupElementIdReverse EETrackJoinBlocks = 0x000000E9
lookupElementIdReverse EETrackJoinUID = 0x000000ED
lookupElementIdReverse EETrickTrackUID = 0x000000C0
lookupElementIdReverse EETrickTrackSegmentUID = 0x000000C1
lookupElementIdReverse EETrickTrackFlag = 0x000000C6
lookupElementIdReverse EETrickMasterTrackUID = 0x000000C7
lookupElementIdReverse EETrickMasterTrackSegmentUID = 0x000000C4
lookupElementIdReverse EEContentEncodings = 0x00006D80
lookupElementIdReverse EEContentEncoding = 0x00006240
lookupElementIdReverse EEContentEncodingOrder = 0x00005031
lookupElementIdReverse EEContentEncodingScope = 0x00005032
lookupElementIdReverse EEContentEncodingType = 0x00005033
lookupElementIdReverse EEContentCompression = 0x00005034
lookupElementIdReverse EEContentCompAlgo = 0x00004254
lookupElementIdReverse EEContentCompSettings = 0x00004255
lookupElementIdReverse EEContentEncryption = 0x00005035
lookupElementIdReverse EEContentEncAlgo = 0x000047E1
lookupElementIdReverse EEContentEncKeyID = 0x000047E2
lookupElementIdReverse EEContentSignature = 0x000047E3
lookupElementIdReverse EEContentSigKeyID = 0x000047E4
lookupElementIdReverse EEContentSigAlgo = 0x000047E5
lookupElementIdReverse EEContentSigHashAlgo = 0x000047E6
lookupElementIdReverse EECues = 0x1C53BB6B
lookupElementIdReverse EECuePoint = 0x000000BB
lookupElementIdReverse EECueTime = 0x000000B3
lookupElementIdReverse EECueTrackPositions = 0x000000B7
lookupElementIdReverse EECueTrack = 0x000000F7
lookupElementIdReverse EECueClusterPosition = 0x000000F1
lookupElementIdReverse EECueBlockNumber = 0x00005378
lookupElementIdReverse EECueCodecState = 0x000000EA
lookupElementIdReverse EECueReference = 0x000000DB
lookupElementIdReverse EECueRefTime = 0x00000096
lookupElementIdReverse EECueRefCluster = 0x00000097
lookupElementIdReverse EECueRefNumber = 0x0000535F
lookupElementIdReverse EECueRefCodecState = 0x000000EB
lookupElementIdReverse EEAttachments = 0x1941A469
lookupElementIdReverse EEAttachedFile = 0x000061A7
lookupElementIdReverse EEFileDescription = 0x0000467E
lookupElementIdReverse EEFileName = 0x0000466E
lookupElementIdReverse EEFileMimeType = 0x00004660
lookupElementIdReverse EEFileData = 0x0000465C
lookupElementIdReverse EEFileUID = 0x000046AE
lookupElementIdReverse EEFileReferral = 0x00004675
lookupElementIdReverse EEFileUsedStartTime = 0x00004661
lookupElementIdReverse EEFileUsedEndTime = 0x00004662
lookupElementIdReverse EEChapters = 0x1043A770
lookupElementIdReverse EEEditionEntry = 0x000045B9
lookupElementIdReverse EEEditionUID = 0x000045BC
lookupElementIdReverse EEEditionFlagHidden = 0x000045BD
lookupElementIdReverse EEEditionFlagDefault = 0x000045DB
lookupElementIdReverse EEEditionFlagOrdered = 0x000045DD
lookupElementIdReverse EEChapterAtom = 0x000000B6
lookupElementIdReverse EEChapterUID = 0x000073C4
lookupElementIdReverse EEChapterTimeStart = 0x00000091
lookupElementIdReverse EEChapterTimeEnd = 0x00000092
lookupElementIdReverse EEChapterFlagHidden = 0x00000098
lookupElementIdReverse EEChapterFlagEnabled = 0x00004598
lookupElementIdReverse EEChapterSegmentUID = 0x00006E67
lookupElementIdReverse EEChapterSegmentEditionUID = 0x00006EBC
lookupElementIdReverse EEChapterPhysicalEquiv = 0x000063C3
lookupElementIdReverse EEChapterTrack = 0x0000008F
lookupElementIdReverse EEChapterTrackNumber = 0x00000089
lookupElementIdReverse EEChapterDisplay = 0x00000080
lookupElementIdReverse EEChapString = 0x00000085
lookupElementIdReverse EEChapLanguage = 0x0000437C
lookupElementIdReverse EEChapCountry = 0x0000437E
lookupElementIdReverse EEChapProcess = 0x00006944
lookupElementIdReverse EEChapProcessCodecID = 0x00006955
lookupElementIdReverse EEChapProcessPrivate = 0x0000450D
lookupElementIdReverse EEChapProcessCommand = 0x00006911
lookupElementIdReverse EEChapProcessTime = 0x00006922
lookupElementIdReverse EEChapProcessData = 0x00006933
lookupElementIdReverse EETags = 0x1254C367
lookupElementIdReverse EETag = 0x00007373
lookupElementIdReverse EETargets = 0x000063C0
lookupElementIdReverse EETargetTypeValue = 0x000068CA
lookupElementIdReverse EETargetType = 0x000063CA
lookupElementIdReverse EETagTrackUID = 0x000063C5
lookupElementIdReverse EETagEditionUID = 0x000063C9
lookupElementIdReverse EETagChapterUID = 0x000063C4
lookupElementIdReverse EETagAttachmentUID = 0x000063C6
lookupElementIdReverse EESimpleTag = 0x000067C8
lookupElementIdReverse EETagName = 0x000045A3
lookupElementIdReverse EETagLanguage = 0x0000447A
lookupElementIdReverse EETagDefault = 0x00004484
lookupElementIdReverse EETagString = 0x00004487
lookupElementIdReverse EETagBinary = 0x00004485
lookupElementIdReverse (EEUnknown id_) = id_

lookupElementId 0x1A45DFA3 = EEEBML
lookupElementId 0x00004286 = EEEBMLVersion
lookupElementId 0x000042F7 = EEEBMLReadVersion
lookupElementId 0x000042F2 = EEEBMLMaxIDLength
lookupElementId 0x000042F3 = EEEBMLMaxSizeLength
lookupElementId 0x00004282 = EEDocType
lookupElementId 0x00004287 = EEDocTypeVersion
lookupElementId 0x00004285 = EEDocTypeReadVersion
lookupElementId 0x000000EC = EEVoid
lookupElementId 0x000000BF = EECRC32
lookupElementId 0x1B538667 = EESignatureSlot
lookupElementId 0x00007E8A = EESignatureAlgo
lookupElementId 0x00007E9A = EESignatureHash
lookupElementId 0x00007EA5 = EESignaturePublicKey
lookupElementId 0x00007EB5 = EESignature
lookupElementId 0x00007E5B = EESignatureElements
lookupElementId 0x00007E7B = EESignatureElementList
lookupElementId 0x00006532 = EESignedElement
lookupElementId 0x18538067 = EESegment
lookupElementId 0x114D9B74 = EESeekHead
lookupElementId 0x00004DBB = EESeek
lookupElementId 0x000053AB = EESeekID
lookupElementId 0x000053AC = EESeekPosition
lookupElementId 0x1549A966 = EEInfo
lookupElementId 0x000073A4 = EESegmentUID
lookupElementId 0x00007384 = EESegmentFilename
lookupElementId 0x003CB923 = EEPrevUID
lookupElementId 0x003C83AB = EEPrevFilename
lookupElementId 0x003EB923 = EENextUID
lookupElementId 0x003E83BB = EENextFilename
lookupElementId 0x00004444 = EESegmentFamily
lookupElementId 0x00006924 = EEChapterTranslate
lookupElementId 0x000069FC = EEChapterTranslateEditionUID
lookupElementId 0x000069BF = EEChapterTranslateCodec
lookupElementId 0x000069A5 = EEChapterTranslateID
lookupElementId 0x002AD7B1 = EETimecodeScale
lookupElementId 0x00004489 = EEDuration
lookupElementId 0x00004461 = EEDateUTC
lookupElementId 0x00007BA9 = EETitle
lookupElementId 0x00004D80 = EEMuxingApp
lookupElementId 0x00005741 = EEWritingApp
lookupElementId 0x1F43B675 = EECluster
lookupElementId 0x000000E7 = EETimecode
lookupElementId 0x00005854 = EESilentTracks
lookupElementId 0x000058D7 = EESilentTrackNumber
lookupElementId 0x000000A7 = EEPosition
lookupElementId 0x000000AB = EEPrevSize
lookupElementId 0x000000A3 = EESimpleBlock
lookupElementId 0x000000A0 = EEBlockGroup
lookupElementId 0x000000A1 = EEBlock
lookupElementId 0x000000A2 = EEBlockVirtual
lookupElementId 0x000075A1 = EEBlockAdditions
lookupElementId 0x000000A6 = EEBlockMore
lookupElementId 0x000000EE = EEBlockAddID
lookupElementId 0x000000A5 = EEBlockAdditional
lookupElementId 0x0000009B = EEBlockDuration
lookupElementId 0x000000FA = EEReferencePriority
lookupElementId 0x000000FB = EEReferenceBlock
lookupElementId 0x000000FD = EEReferenceVirtual
lookupElementId 0x000000A4 = EECodecState
lookupElementId 0x0000008E = EESlices
lookupElementId 0x000000E8 = EETimeSlice
lookupElementId 0x000000CC = EELaceNumber
lookupElementId 0x000000CD = EEFrameNumber
lookupElementId 0x000000CB = EEBlockAdditionID
lookupElementId 0x000000CE = EEDelay
lookupElementId 0x000000CF = EESliceDuration
lookupElementId 0x000000C8 = EEReferenceFrame
lookupElementId 0x000000C9 = EEReferenceOffset
lookupElementId 0x000000CA = EEReferenceTimeCode
lookupElementId 0x000000AF = EEEncryptedBlock
lookupElementId 0x1654AE6B = EETracks
lookupElementId 0x000000AE = EETrackEntry
lookupElementId 0x000000D7 = EETrackNumber
lookupElementId 0x000073C5 = EETrackUID
lookupElementId 0x00000083 = EETrackType
lookupElementId 0x000000B9 = EEFlagEnabled
lookupElementId 0x00000088 = EEFlagDefault
lookupElementId 0x000055AA = EEFlagForced
lookupElementId 0x0000009C = EEFlagLacing
lookupElementId 0x00006DE7 = EEMinCache
lookupElementId 0x00006DF8 = EEMaxCache
lookupElementId 0x0023E383 = EEDefaultDuration
lookupElementId 0x0023314F = EETrackTimecodeScale
lookupElementId 0x0000537F = EETrackOffset
lookupElementId 0x000055EE = EEMaxBlockAdditionID
lookupElementId 0x0000536E = EEName
lookupElementId 0x0022B59C = EELanguage
lookupElementId 0x00000086 = EECodecID
lookupElementId 0x000063A2 = EECodecPrivate
lookupElementId 0x00258688 = EECodecName
lookupElementId 0x00007446 = EEAttachmentLink
lookupElementId 0x003A9697 = EECodecSettings
lookupElementId 0x003B4040 = EECodecInfoURL
lookupElementId 0x0026B240 = EECodecDownloadURL
lookupElementId 0x000000AA = EECodecDecodeAll
lookupElementId 0x00006FAB = EETrackOverlay
lookupElementId 0x00006624 = EETrackTranslate
lookupElementId 0x000066FC = EETrackTranslateEditionUID
lookupElementId 0x000066BF = EETrackTranslateCodec
lookupElementId 0x000066A5 = EETrackTranslateTrackID
lookupElementId 0x000000E0 = EEVideo
lookupElementId 0x0000009A = EEFlagInterlaced
lookupElementId 0x000053B8 = EEStereoMode
lookupElementId 0x000053B9 = EEOldStereoMode
lookupElementId 0x000000B0 = EEPixelWidth
lookupElementId 0x000000BA = EEPixelHeight
lookupElementId 0x000054AA = EEPixelCropBottom
lookupElementId 0x000054BB = EEPixelCropTop
lookupElementId 0x000054CC = EEPixelCropLeft
lookupElementId 0x000054DD = EEPixelCropRight
lookupElementId 0x000054B0 = EEDisplayWidth
lookupElementId 0x000054BA = EEDisplayHeight
lookupElementId 0x000054B2 = EEDisplayUnit
lookupElementId 0x000054B3 = EEAspectRatioType
lookupElementId 0x002EB524 = EEColourSpace
lookupElementId 0x002FB523 = EEGammaValue
lookupElementId 0x002383E3 = EEFrameRate
lookupElementId 0x000000E1 = EEAudio
lookupElementId 0x000000B5 = EESamplingFrequency
lookupElementId 0x000078B5 = EEOutputSamplingFrequency
lookupElementId 0x0000009F = EEChannels
lookupElementId 0x00007D7B = EEChannelPositions
lookupElementId 0x00006264 = EEBitDepth
lookupElementId 0x000000E2 = EETrackOperation
lookupElementId 0x000000E3 = EETrackCombinePlanes
lookupElementId 0x000000E4 = EETrackPlane
lookupElementId 0x000000E5 = EETrackPlaneUID
lookupElementId 0x000000E6 = EETrackPlaneType
lookupElementId 0x000000E9 = EETrackJoinBlocks
lookupElementId 0x000000ED = EETrackJoinUID
lookupElementId 0x000000C0 = EETrickTrackUID
lookupElementId 0x000000C1 = EETrickTrackSegmentUID
lookupElementId 0x000000C6 = EETrickTrackFlag
lookupElementId 0x000000C7 = EETrickMasterTrackUID
lookupElementId 0x000000C4 = EETrickMasterTrackSegmentUID
lookupElementId 0x00006D80 = EEContentEncodings
lookupElementId 0x00006240 = EEContentEncoding
lookupElementId 0x00005031 = EEContentEncodingOrder
lookupElementId 0x00005032 = EEContentEncodingScope
lookupElementId 0x00005033 = EEContentEncodingType
lookupElementId 0x00005034 = EEContentCompression
lookupElementId 0x00004254 = EEContentCompAlgo
lookupElementId 0x00004255 = EEContentCompSettings
lookupElementId 0x00005035 = EEContentEncryption
lookupElementId 0x000047E1 = EEContentEncAlgo
lookupElementId 0x000047E2 = EEContentEncKeyID
lookupElementId 0x000047E3 = EEContentSignature
lookupElementId 0x000047E4 = EEContentSigKeyID
lookupElementId 0x000047E5 = EEContentSigAlgo
lookupElementId 0x000047E6 = EEContentSigHashAlgo
lookupElementId 0x1C53BB6B = EECues
lookupElementId 0x000000BB = EECuePoint
lookupElementId 0x000000B3 = EECueTime
lookupElementId 0x000000B7 = EECueTrackPositions
lookupElementId 0x000000F7 = EECueTrack
lookupElementId 0x000000F1 = EECueClusterPosition
lookupElementId 0x00005378 = EECueBlockNumber
lookupElementId 0x000000EA = EECueCodecState
lookupElementId 0x000000DB = EECueReference
lookupElementId 0x00000096 = EECueRefTime
lookupElementId 0x00000097 = EECueRefCluster
lookupElementId 0x0000535F = EECueRefNumber
lookupElementId 0x000000EB = EECueRefCodecState
lookupElementId 0x1941A469 = EEAttachments
lookupElementId 0x000061A7 = EEAttachedFile
lookupElementId 0x0000467E = EEFileDescription
lookupElementId 0x0000466E = EEFileName
lookupElementId 0x00004660 = EEFileMimeType
lookupElementId 0x0000465C = EEFileData
lookupElementId 0x000046AE = EEFileUID
lookupElementId 0x00004675 = EEFileReferral
lookupElementId 0x00004661 = EEFileUsedStartTime
lookupElementId 0x00004662 = EEFileUsedEndTime
lookupElementId 0x1043A770 = EEChapters
lookupElementId 0x000045B9 = EEEditionEntry
lookupElementId 0x000045BC = EEEditionUID
lookupElementId 0x000045BD = EEEditionFlagHidden
lookupElementId 0x000045DB = EEEditionFlagDefault
lookupElementId 0x000045DD = EEEditionFlagOrdered
lookupElementId 0x000000B6 = EEChapterAtom
lookupElementId 0x000073C4 = EEChapterUID
lookupElementId 0x00000091 = EEChapterTimeStart
lookupElementId 0x00000092 = EEChapterTimeEnd
lookupElementId 0x00000098 = EEChapterFlagHidden
lookupElementId 0x00004598 = EEChapterFlagEnabled
lookupElementId 0x00006E67 = EEChapterSegmentUID
lookupElementId 0x00006EBC = EEChapterSegmentEditionUID
lookupElementId 0x000063C3 = EEChapterPhysicalEquiv
lookupElementId 0x0000008F = EEChapterTrack
lookupElementId 0x00000089 = EEChapterTrackNumber
lookupElementId 0x00000080 = EEChapterDisplay
lookupElementId 0x00000085 = EEChapString
lookupElementId 0x0000437C = EEChapLanguage
lookupElementId 0x0000437E = EEChapCountry
lookupElementId 0x00006944 = EEChapProcess
lookupElementId 0x00006955 = EEChapProcessCodecID
lookupElementId 0x0000450D = EEChapProcessPrivate
lookupElementId 0x00006911 = EEChapProcessCommand
lookupElementId 0x00006922 = EEChapProcessTime
lookupElementId 0x00006933 = EEChapProcessData
lookupElementId 0x1254C367 = EETags
lookupElementId 0x00007373 = EETag
lookupElementId 0x000063C0 = EETargets
lookupElementId 0x000068CA = EETargetTypeValue
lookupElementId 0x000063CA = EETargetType
lookupElementId 0x000063C5 = EETagTrackUID
lookupElementId 0x000063C9 = EETagEditionUID
lookupElementId 0x000063C4 = EETagChapterUID
lookupElementId 0x000063C6 = EETagAttachmentUID
lookupElementId 0x000067C8 = EESimpleTag
lookupElementId 0x000045A3 = EETagName
lookupElementId 0x0000447A = EETagLanguage
lookupElementId 0x00004484 = EETagDefault
lookupElementId 0x00004487 = EETagString
lookupElementId 0x00004485 = EETagBinary
lookupElementId id_ = EEUnknown id_

lookupElementType EEEBML                        = ETMaster
lookupElementType EEEBMLVersion                 = ETUnsigned
lookupElementType EEEBMLReadVersion             = ETUnsigned
lookupElementType EEEBMLMaxIDLength             = ETUnsigned
lookupElementType EEEBMLMaxSizeLength           = ETUnsigned
lookupElementType EEDocType                     = ETTextAscii
lookupElementType EEDocTypeVersion              = ETUnsigned
lookupElementType EEDocTypeReadVersion          = ETUnsigned
lookupElementType EEVoid                        = ETBinary
lookupElementType EECRC32                       = ETBinary
lookupElementType EESignatureSlot               = ETMaster
lookupElementType EESignatureAlgo               = ETUnsigned
lookupElementType EESignatureHash               = ETUnsigned
lookupElementType EESignaturePublicKey          = ETBinary
lookupElementType EESignature                   = ETBinary
lookupElementType EESignatureElements           = ETMaster
lookupElementType EESignatureElementList        = ETMaster
lookupElementType EESignedElement               = ETBinary
lookupElementType EESegment                     = ETFlatten
lookupElementType EESeekHead                    = ETMaster
lookupElementType EESeek                        = ETMaster
lookupElementType EESeekID                      = ETBinary
lookupElementType EESeekPosition                = ETUnsigned
lookupElementType EEInfo                        = ETMaster
lookupElementType EESegmentUID                  = ETBinary
lookupElementType EESegmentFilename             = ETTextUtf8
lookupElementType EEPrevUID                     = ETBinary
lookupElementType EEPrevFilename                = ETTextUtf8
lookupElementType EENextUID                     = ETBinary
lookupElementType EENextFilename                = ETTextUtf8
lookupElementType EESegmentFamily               = ETBinary
lookupElementType EEChapterTranslate            = ETMaster
lookupElementType EEChapterTranslateEditionUID  = ETUnsigned
lookupElementType EEChapterTranslateCodec       = ETUnsigned
lookupElementType EEChapterTranslateID          = ETBinary
lookupElementType EETimecodeScale               = ETUnsigned
lookupElementType EEDuration                    = ETFloat
lookupElementType EEDateUTC                     = ETDate
lookupElementType EETitle                       = ETTextUtf8
lookupElementType EEMuxingApp                   = ETTextUtf8
lookupElementType EEWritingApp                  = ETTextUtf8
lookupElementType EECluster                     = ETFlatten
lookupElementType EETimecode                    = ETUnsigned
lookupElementType EESilentTracks                = ETMaster
lookupElementType EESilentTrackNumber           = ETUnsigned
lookupElementType EEPosition                    = ETUnsigned
lookupElementType EEPrevSize                    = ETUnsigned
lookupElementType EESimpleBlock                 = ETBinary
lookupElementType EEBlockGroup                  = ETMaster
lookupElementType EEBlock                       = ETBinary
lookupElementType EEBlockVirtual                = ETBinary
lookupElementType EEBlockAdditions              = ETMaster
lookupElementType EEBlockMore                   = ETMaster
lookupElementType EEBlockAddID                  = ETUnsigned
lookupElementType EEBlockAdditional             = ETBinary
lookupElementType EEBlockDuration               = ETUnsigned
lookupElementType EEReferencePriority           = ETUnsigned
lookupElementType EEReferenceBlock              = ETSigned
lookupElementType EEReferenceVirtual            = ETSigned
lookupElementType EECodecState                  = ETBinary
lookupElementType EESlices                      = ETMaster
lookupElementType EETimeSlice                   = ETMaster
lookupElementType EELaceNumber                  = ETUnsigned
lookupElementType EEFrameNumber                 = ETUnsigned
lookupElementType EEBlockAdditionID             = ETUnsigned
lookupElementType EEDelay                       = ETUnsigned
lookupElementType EESliceDuration               = ETUnsigned
lookupElementType EEReferenceFrame              = ETMaster
lookupElementType EEReferenceOffset             = ETUnsigned
lookupElementType EEReferenceTimeCode           = ETUnsigned
lookupElementType EEEncryptedBlock              = ETBinary
lookupElementType EETracks                      = ETMaster
lookupElementType EETrackEntry                  = ETMaster
lookupElementType EETrackNumber                 = ETUnsigned
lookupElementType EETrackUID                    = ETUnsigned
lookupElementType EETrackType                   = ETUnsigned
lookupElementType EEFlagEnabled                 = ETUnsigned
lookupElementType EEFlagDefault                 = ETUnsigned
lookupElementType EEFlagForced                  = ETUnsigned
lookupElementType EEFlagLacing                  = ETUnsigned
lookupElementType EEMinCache                    = ETUnsigned
lookupElementType EEMaxCache                    = ETUnsigned
lookupElementType EEDefaultDuration             = ETUnsigned
lookupElementType EETrackTimecodeScale          = ETFloat
lookupElementType EETrackOffset                 = ETSigned
lookupElementType EEMaxBlockAdditionID          = ETUnsigned
lookupElementType EEName                        = ETTextUtf8
lookupElementType EELanguage                    = ETTextAscii
lookupElementType EECodecID                     = ETTextAscii
lookupElementType EECodecPrivate                = ETBinary
lookupElementType EECodecName                   = ETTextUtf8
lookupElementType EEAttachmentLink              = ETUnsigned
lookupElementType EECodecSettings               = ETTextUtf8
lookupElementType EECodecInfoURL                = ETTextAscii
lookupElementType EECodecDownloadURL            = ETTextAscii
lookupElementType EECodecDecodeAll              = ETUnsigned
lookupElementType EETrackOverlay                = ETUnsigned
lookupElementType EETrackTranslate              = ETMaster
lookupElementType EETrackTranslateEditionUID    = ETUnsigned
lookupElementType EETrackTranslateCodec         = ETUnsigned
lookupElementType EETrackTranslateTrackID       = ETBinary
lookupElementType EEVideo                       = ETMaster
lookupElementType EEFlagInterlaced              = ETUnsigned
lookupElementType EEStereoMode                  = ETUnsigned
lookupElementType EEOldStereoMode               = ETUnsigned
lookupElementType EEPixelWidth                  = ETUnsigned
lookupElementType EEPixelHeight                 = ETUnsigned
lookupElementType EEPixelCropBottom             = ETUnsigned
lookupElementType EEPixelCropTop                = ETUnsigned
lookupElementType EEPixelCropLeft               = ETUnsigned
lookupElementType EEPixelCropRight              = ETUnsigned
lookupElementType EEDisplayWidth                = ETUnsigned
lookupElementType EEDisplayHeight               = ETUnsigned
lookupElementType EEDisplayUnit                 = ETUnsigned
lookupElementType EEAspectRatioType             = ETUnsigned
lookupElementType EEColourSpace                 = ETBinary
lookupElementType EEGammaValue                  = ETFloat
lookupElementType EEFrameRate                   = ETFloat
lookupElementType EEAudio                       = ETMaster
lookupElementType EESamplingFrequency           = ETFloat
lookupElementType EEOutputSamplingFrequency     = ETFloat
lookupElementType EEChannels                    = ETUnsigned
lookupElementType EEChannelPositions            = ETBinary
lookupElementType EEBitDepth                    = ETUnsigned
lookupElementType EETrackOperation              = ETMaster
lookupElementType EETrackCombinePlanes          = ETMaster
lookupElementType EETrackPlane                  = ETMaster
lookupElementType EETrackPlaneUID               = ETUnsigned
lookupElementType EETrackPlaneType              = ETUnsigned
lookupElementType EETrackJoinBlocks             = ETMaster
lookupElementType EETrackJoinUID                = ETUnsigned
lookupElementType EETrickTrackUID               = ETUnsigned
lookupElementType EETrickTrackSegmentUID        = ETBinary
lookupElementType EETrickTrackFlag              = ETUnsigned
lookupElementType EETrickMasterTrackUID         = ETUnsigned
lookupElementType EETrickMasterTrackSegmentUID  = ETBinary
lookupElementType EEContentEncodings            = ETMaster
lookupElementType EEContentEncoding             = ETMaster
lookupElementType EEContentEncodingOrder        = ETUnsigned
lookupElementType EEContentEncodingScope        = ETUnsigned
lookupElementType EEContentEncodingType         = ETUnsigned
lookupElementType EEContentCompression          = ETMaster
lookupElementType EEContentCompAlgo             = ETUnsigned
lookupElementType EEContentCompSettings         = ETBinary
lookupElementType EEContentEncryption           = ETMaster
lookupElementType EEContentEncAlgo              = ETUnsigned
lookupElementType EEContentEncKeyID             = ETBinary
lookupElementType EEContentSignature            = ETBinary
lookupElementType EEContentSigKeyID             = ETBinary
lookupElementType EEContentSigAlgo              = ETUnsigned
lookupElementType EEContentSigHashAlgo          = ETUnsigned
lookupElementType EECues                        = ETMaster
lookupElementType EECuePoint                    = ETMaster
lookupElementType EECueTime                     = ETUnsigned
lookupElementType EECueTrackPositions           = ETMaster
lookupElementType EECueTrack                    = ETUnsigned
lookupElementType EECueClusterPosition          = ETUnsigned
lookupElementType EECueBlockNumber              = ETUnsigned
lookupElementType EECueCodecState               = ETUnsigned
lookupElementType EECueReference                = ETMaster
lookupElementType EECueRefTime                  = ETUnsigned
lookupElementType EECueRefCluster               = ETUnsigned
lookupElementType EECueRefNumber                = ETUnsigned
lookupElementType EECueRefCodecState            = ETUnsigned
lookupElementType EEAttachments                 = ETMaster
lookupElementType EEAttachedFile                = ETMaster
lookupElementType EEFileDescription             = ETTextUtf8
lookupElementType EEFileName                    = ETTextUtf8
lookupElementType EEFileMimeType                = ETTextAscii
lookupElementType EEFileData                    = ETBinary
lookupElementType EEFileUID                     = ETUnsigned
lookupElementType EEFileReferral                = ETBinary
lookupElementType EEFileUsedStartTime           = ETUnsigned
lookupElementType EEFileUsedEndTime             = ETUnsigned
lookupElementType EEChapters                    = ETMaster
lookupElementType EEEditionEntry                = ETMaster
lookupElementType EEEditionUID                  = ETUnsigned
lookupElementType EEEditionFlagHidden           = ETUnsigned
lookupElementType EEEditionFlagDefault          = ETUnsigned
lookupElementType EEEditionFlagOrdered          = ETUnsigned
lookupElementType EEChapterAtom                 = ETMaster
lookupElementType EEChapterUID                  = ETUnsigned
lookupElementType EEChapterTimeStart            = ETUnsigned
lookupElementType EEChapterTimeEnd              = ETUnsigned
lookupElementType EEChapterFlagHidden           = ETUnsigned
lookupElementType EEChapterFlagEnabled          = ETUnsigned
lookupElementType EEChapterSegmentUID           = ETBinary
lookupElementType EEChapterSegmentEditionUID    = ETUnsigned
lookupElementType EEChapterPhysicalEquiv        = ETUnsigned
lookupElementType EEChapterTrack                = ETMaster
lookupElementType EEChapterTrackNumber          = ETUnsigned
lookupElementType EEChapterDisplay              = ETMaster
lookupElementType EEChapString                  = ETTextUtf8
lookupElementType EEChapLanguage                = ETTextAscii
lookupElementType EEChapCountry                 = ETTextAscii
lookupElementType EEChapProcess                 = ETMaster
lookupElementType EEChapProcessCodecID          = ETUnsigned
lookupElementType EEChapProcessPrivate          = ETBinary
lookupElementType EEChapProcessCommand          = ETMaster
lookupElementType EEChapProcessTime             = ETUnsigned
lookupElementType EEChapProcessData             = ETBinary
lookupElementType EETags                        = ETMaster
lookupElementType EETag                         = ETMaster
lookupElementType EETargets                     = ETMaster
lookupElementType EETargetTypeValue             = ETUnsigned
lookupElementType EETargetType                  = ETTextAscii
lookupElementType EETagTrackUID                 = ETUnsigned
lookupElementType EETagEditionUID               = ETUnsigned
lookupElementType EETagChapterUID               = ETUnsigned
lookupElementType EETagAttachmentUID            = ETUnsigned
lookupElementType EESimpleTag                   = ETMaster
lookupElementType EETagName                     = ETTextUtf8
lookupElementType EETagLanguage                 = ETTextAscii
lookupElementType EETagDefault                  = ETUnsigned
lookupElementType EETagString                   = ETTextUtf8
lookupElementType EETagBinary                   = ETBinary
lookupElementType (EEUnknown _)                 = ETUnknown
 
