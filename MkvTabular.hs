module MkvTabular (
    ElementType(..),
    EbmlElementID,
    ElementClass(..),
    lookupElementId,
    lookupElementType
    )where

type EbmlElementID = Integer

data ElementType =
        ET_Master |
        ET_Unsigned |
        ET_Signed |
        ET_TextAscii |
        ET_TextUtf8 |
        ET_Binary |
        ET_Float |
        ET_Date |
        ET_Flatten |
        ET_Unknown
        deriving (Show, Eq, Ord)

lookupElementId :: EbmlElementID -> ElementClass
lookupElementType :: ElementClass -> ElementType

data ElementClass =
    EE_EBML |
    EE_EBMLVersion |
    EE_EBMLReadVersion |
    EE_EBMLMaxIDLength |
    EE_EBMLMaxSizeLength |
    EE_DocType |
    EE_DocTypeVersion |
    EE_DocTypeReadVersion |
    EE_Void |
    EE_CRC32 |
    EE_SignatureSlot |
    EE_SignatureAlgo |
    EE_SignatureHash |
    EE_SignaturePublicKey |
    EE_Signature |
    EE_SignatureElements |
    EE_SignatureElementList |
    EE_SignedElement |
    EE_Segment |
    EE_SeekHead |
    EE_Seek |
    EE_SeekID |
    EE_SeekPosition |
    EE_Info |
    EE_SegmentUID |
    EE_SegmentFilename |
    EE_PrevUID |
    EE_PrevFilename |
    EE_NextUID |
    EE_NextFilename |
    EE_SegmentFamily |
    EE_ChapterTranslate |
    EE_ChapterTranslateEditionUID |
    EE_ChapterTranslateCodec |
    EE_ChapterTranslateID |
    EE_TimecodeScale |
    EE_Duration |
    EE_DateUTC |
    EE_Title |
    EE_MuxingApp |
    EE_WritingApp |
    EE_Cluster |
    EE_Timecode |
    EE_SilentTracks |
    EE_SilentTrackNumber |
    EE_Position |
    EE_PrevSize |
    EE_SimpleBlock |
    EE_BlockGroup |
    EE_Block |
    EE_BlockVirtual |
    EE_BlockAdditions |
    EE_BlockMore |
    EE_BlockAddID |
    EE_BlockAdditional |
    EE_BlockDuration |
    EE_ReferencePriority |
    EE_ReferenceBlock |
    EE_ReferenceVirtual |
    EE_CodecState |
    EE_Slices |
    EE_TimeSlice |
    EE_LaceNumber |
    EE_FrameNumber |
    EE_BlockAdditionID |
    EE_Delay |
    EE_SliceDuration |
    EE_ReferenceFrame |
    EE_ReferenceOffset |
    EE_ReferenceTimeCode |
    EE_EncryptedBlock |
    EE_Tracks |
    EE_TrackEntry |
    EE_TrackNumber |
    EE_TrackUID |
    EE_TrackType |
    EE_FlagEnabled |
    EE_FlagDefault |
    EE_FlagForced |
    EE_FlagLacing |
    EE_MinCache |
    EE_MaxCache |
    EE_DefaultDuration |
    EE_TrackTimecodeScale |
    EE_TrackOffset |
    EE_MaxBlockAdditionID |
    EE_Name |
    EE_Language |
    EE_CodecID |
    EE_CodecPrivate |
    EE_CodecName |
    EE_AttachmentLink |
    EE_CodecSettings |
    EE_CodecInfoURL |
    EE_CodecDownloadURL |
    EE_CodecDecodeAll |
    EE_TrackOverlay |
    EE_TrackTranslate |
    EE_TrackTranslateEditionUID |
    EE_TrackTranslateCodec |
    EE_TrackTranslateTrackID |
    EE_Video |
    EE_FlagInterlaced |
    EE_StereoMode |
    EE_OldStereoMode |
    EE_PixelWidth |
    EE_PixelHeight |
    EE_PixelCropBottom |
    EE_PixelCropTop |
    EE_PixelCropLeft |
    EE_PixelCropRight |
    EE_DisplayWidth |
    EE_DisplayHeight |
    EE_DisplayUnit |
    EE_AspectRatioType |
    EE_ColourSpace |
    EE_GammaValue |
    EE_FrameRate |
    EE_Audio |
    EE_SamplingFrequency |
    EE_OutputSamplingFrequency |
    EE_Channels |
    EE_ChannelPositions |
    EE_BitDepth |
    EE_TrackOperation |
    EE_TrackCombinePlanes |
    EE_TrackPlane |
    EE_TrackPlaneUID |
    EE_TrackPlaneType |
    EE_TrackJoinBlocks |
    EE_TrackJoinUID |
    EE_TrickTrackUID |
    EE_TrickTrackSegmentUID |
    EE_TrickTrackFlag |
    EE_TrickMasterTrackUID |
    EE_TrickMasterTrackSegmentUID |
    EE_ContentEncodings |
    EE_ContentEncoding |
    EE_ContentEncodingOrder |
    EE_ContentEncodingScope |
    EE_ContentEncodingType |
    EE_ContentCompression |
    EE_ContentCompAlgo |
    EE_ContentCompSettings |
    EE_ContentEncryption |
    EE_ContentEncAlgo |
    EE_ContentEncKeyID |
    EE_ContentSignature |
    EE_ContentSigKeyID |
    EE_ContentSigAlgo |
    EE_ContentSigHashAlgo |
    EE_Cues |
    EE_CuePoint |
    EE_CueTime |
    EE_CueTrackPositions |
    EE_CueTrack |
    EE_CueClusterPosition |
    EE_CueBlockNumber |
    EE_CueCodecState |
    EE_CueReference |
    EE_CueRefTime |
    EE_CueRefCluster |
    EE_CueRefNumber |
    EE_CueRefCodecState |
    EE_Attachments |
    EE_AttachedFile |
    EE_FileDescription |
    EE_FileName |
    EE_FileMimeType |
    EE_FileData |
    EE_FileUID |
    EE_FileReferral |
    EE_FileUsedStartTime |
    EE_FileUsedEndTime |
    EE_Chapters |
    EE_EditionEntry |
    EE_EditionUID |
    EE_EditionFlagHidden |
    EE_EditionFlagDefault |
    EE_EditionFlagOrdered |
    EE_ChapterAtom |
    EE_ChapterUID |
    EE_ChapterTimeStart |
    EE_ChapterTimeEnd |
    EE_ChapterFlagHidden |
    EE_ChapterFlagEnabled |
    EE_ChapterSegmentUID |
    EE_ChapterSegmentEditionUID |
    EE_ChapterPhysicalEquiv |
    EE_ChapterTrack |
    EE_ChapterTrackNumber |
    EE_ChapterDisplay |
    EE_ChapString |
    EE_ChapLanguage |
    EE_ChapCountry |
    EE_ChapProcess |
    EE_ChapProcessCodecID |
    EE_ChapProcessPrivate |
    EE_ChapProcessCommand |
    EE_ChapProcessTime |
    EE_ChapProcessData |
    EE_Tags |
    EE_Tag |
    EE_Targets |
    EE_TargetTypeValue |
    EE_TargetType |
    EE_TagTrackUID |
    EE_TagEditionUID |
    EE_TagChapterUID |
    EE_TagAttachmentUID |
    EE_SimpleTag |
    EE_TagName |
    EE_TagLanguage |
    EE_TagDefault |
    EE_TagString |
    EE_TagBinary |
    EE_Unknown EbmlElementID
    deriving (Show, Eq, Ord)

lookupElementId 0x1A45DFA3 = EE_EBML
lookupElementId 0x00004286 = EE_EBMLVersion
lookupElementId 0x000042F7 = EE_EBMLReadVersion
lookupElementId 0x000042F2 = EE_EBMLMaxIDLength
lookupElementId 0x000042F3 = EE_EBMLMaxSizeLength
lookupElementId 0x00004282 = EE_DocType
lookupElementId 0x00004287 = EE_DocTypeVersion
lookupElementId 0x00004285 = EE_DocTypeReadVersion
lookupElementId 0x000000EC = EE_Void
lookupElementId 0x000000BF = EE_CRC32
lookupElementId 0x1B538667 = EE_SignatureSlot
lookupElementId 0x00007E8A = EE_SignatureAlgo
lookupElementId 0x00007E9A = EE_SignatureHash
lookupElementId 0x00007EA5 = EE_SignaturePublicKey
lookupElementId 0x00007EB5 = EE_Signature
lookupElementId 0x00007E5B = EE_SignatureElements
lookupElementId 0x00007E7B = EE_SignatureElementList
lookupElementId 0x00006532 = EE_SignedElement
lookupElementId 0x18538067 = EE_Segment
lookupElementId 0x114D9B74 = EE_SeekHead
lookupElementId 0x00004DBB = EE_Seek
lookupElementId 0x000053AB = EE_SeekID
lookupElementId 0x000053AC = EE_SeekPosition
lookupElementId 0x1549A966 = EE_Info
lookupElementId 0x000073A4 = EE_SegmentUID
lookupElementId 0x00007384 = EE_SegmentFilename
lookupElementId 0x003CB923 = EE_PrevUID
lookupElementId 0x003C83AB = EE_PrevFilename
lookupElementId 0x003EB923 = EE_NextUID
lookupElementId 0x003E83BB = EE_NextFilename
lookupElementId 0x00004444 = EE_SegmentFamily
lookupElementId 0x00006924 = EE_ChapterTranslate
lookupElementId 0x000069FC = EE_ChapterTranslateEditionUID
lookupElementId 0x000069BF = EE_ChapterTranslateCodec
lookupElementId 0x000069A5 = EE_ChapterTranslateID
lookupElementId 0x002AD7B1 = EE_TimecodeScale
lookupElementId 0x00004489 = EE_Duration
lookupElementId 0x00004461 = EE_DateUTC
lookupElementId 0x00007BA9 = EE_Title
lookupElementId 0x00004D80 = EE_MuxingApp
lookupElementId 0x00005741 = EE_WritingApp
lookupElementId 0x1F43B675 = EE_Cluster
lookupElementId 0x000000E7 = EE_Timecode
lookupElementId 0x00005854 = EE_SilentTracks
lookupElementId 0x000058D7 = EE_SilentTrackNumber
lookupElementId 0x000000A7 = EE_Position
lookupElementId 0x000000AB = EE_PrevSize
lookupElementId 0x000000A3 = EE_SimpleBlock
lookupElementId 0x000000A0 = EE_BlockGroup
lookupElementId 0x000000A1 = EE_Block
lookupElementId 0x000000A2 = EE_BlockVirtual
lookupElementId 0x000075A1 = EE_BlockAdditions
lookupElementId 0x000000A6 = EE_BlockMore
lookupElementId 0x000000EE = EE_BlockAddID
lookupElementId 0x000000A5 = EE_BlockAdditional
lookupElementId 0x0000009B = EE_BlockDuration
lookupElementId 0x000000FA = EE_ReferencePriority
lookupElementId 0x000000FB = EE_ReferenceBlock
lookupElementId 0x000000FD = EE_ReferenceVirtual
lookupElementId 0x000000A4 = EE_CodecState
lookupElementId 0x0000008E = EE_Slices
lookupElementId 0x000000E8 = EE_TimeSlice
lookupElementId 0x000000CC = EE_LaceNumber
lookupElementId 0x000000CD = EE_FrameNumber
lookupElementId 0x000000CB = EE_BlockAdditionID
lookupElementId 0x000000CE = EE_Delay
lookupElementId 0x000000CF = EE_SliceDuration
lookupElementId 0x000000C8 = EE_ReferenceFrame
lookupElementId 0x000000C9 = EE_ReferenceOffset
lookupElementId 0x000000CA = EE_ReferenceTimeCode
lookupElementId 0x000000AF = EE_EncryptedBlock
lookupElementId 0x1654AE6B = EE_Tracks
lookupElementId 0x000000AE = EE_TrackEntry
lookupElementId 0x000000D7 = EE_TrackNumber
lookupElementId 0x000073C5 = EE_TrackUID
lookupElementId 0x00000083 = EE_TrackType
lookupElementId 0x000000B9 = EE_FlagEnabled
lookupElementId 0x00000088 = EE_FlagDefault
lookupElementId 0x000055AA = EE_FlagForced
lookupElementId 0x0000009C = EE_FlagLacing
lookupElementId 0x00006DE7 = EE_MinCache
lookupElementId 0x00006DF8 = EE_MaxCache
lookupElementId 0x0023E383 = EE_DefaultDuration
lookupElementId 0x0023314F = EE_TrackTimecodeScale
lookupElementId 0x0000537F = EE_TrackOffset
lookupElementId 0x000055EE = EE_MaxBlockAdditionID
lookupElementId 0x0000536E = EE_Name
lookupElementId 0x0022B59C = EE_Language
lookupElementId 0x00000086 = EE_CodecID
lookupElementId 0x000063A2 = EE_CodecPrivate
lookupElementId 0x00258688 = EE_CodecName
lookupElementId 0x00007446 = EE_AttachmentLink
lookupElementId 0x003A9697 = EE_CodecSettings
lookupElementId 0x003B4040 = EE_CodecInfoURL
lookupElementId 0x0026B240 = EE_CodecDownloadURL
lookupElementId 0x000000AA = EE_CodecDecodeAll
lookupElementId 0x00006FAB = EE_TrackOverlay
lookupElementId 0x00006624 = EE_TrackTranslate
lookupElementId 0x000066FC = EE_TrackTranslateEditionUID
lookupElementId 0x000066BF = EE_TrackTranslateCodec
lookupElementId 0x000066A5 = EE_TrackTranslateTrackID
lookupElementId 0x000000E0 = EE_Video
lookupElementId 0x0000009A = EE_FlagInterlaced
lookupElementId 0x000053B8 = EE_StereoMode
lookupElementId 0x000053B9 = EE_OldStereoMode
lookupElementId 0x000000B0 = EE_PixelWidth
lookupElementId 0x000000BA = EE_PixelHeight
lookupElementId 0x000054AA = EE_PixelCropBottom
lookupElementId 0x000054BB = EE_PixelCropTop
lookupElementId 0x000054CC = EE_PixelCropLeft
lookupElementId 0x000054DD = EE_PixelCropRight
lookupElementId 0x000054B0 = EE_DisplayWidth
lookupElementId 0x000054BA = EE_DisplayHeight
lookupElementId 0x000054B2 = EE_DisplayUnit
lookupElementId 0x000054B3 = EE_AspectRatioType
lookupElementId 0x002EB524 = EE_ColourSpace
lookupElementId 0x002FB523 = EE_GammaValue
lookupElementId 0x002383E3 = EE_FrameRate
lookupElementId 0x000000E1 = EE_Audio
lookupElementId 0x000000B5 = EE_SamplingFrequency
lookupElementId 0x000078B5 = EE_OutputSamplingFrequency
lookupElementId 0x0000009F = EE_Channels
lookupElementId 0x00007D7B = EE_ChannelPositions
lookupElementId 0x00006264 = EE_BitDepth
lookupElementId 0x000000E2 = EE_TrackOperation
lookupElementId 0x000000E3 = EE_TrackCombinePlanes
lookupElementId 0x000000E4 = EE_TrackPlane
lookupElementId 0x000000E5 = EE_TrackPlaneUID
lookupElementId 0x000000E6 = EE_TrackPlaneType
lookupElementId 0x000000E9 = EE_TrackJoinBlocks
lookupElementId 0x000000ED = EE_TrackJoinUID
lookupElementId 0x000000C0 = EE_TrickTrackUID
lookupElementId 0x000000C1 = EE_TrickTrackSegmentUID
lookupElementId 0x000000C6 = EE_TrickTrackFlag
lookupElementId 0x000000C7 = EE_TrickMasterTrackUID
lookupElementId 0x000000C4 = EE_TrickMasterTrackSegmentUID
lookupElementId 0x00006D80 = EE_ContentEncodings
lookupElementId 0x00006240 = EE_ContentEncoding
lookupElementId 0x00005031 = EE_ContentEncodingOrder
lookupElementId 0x00005032 = EE_ContentEncodingScope
lookupElementId 0x00005033 = EE_ContentEncodingType
lookupElementId 0x00005034 = EE_ContentCompression
lookupElementId 0x00004254 = EE_ContentCompAlgo
lookupElementId 0x00004255 = EE_ContentCompSettings
lookupElementId 0x00005035 = EE_ContentEncryption
lookupElementId 0x000047E1 = EE_ContentEncAlgo
lookupElementId 0x000047E2 = EE_ContentEncKeyID
lookupElementId 0x000047E3 = EE_ContentSignature
lookupElementId 0x000047E4 = EE_ContentSigKeyID
lookupElementId 0x000047E5 = EE_ContentSigAlgo
lookupElementId 0x000047E6 = EE_ContentSigHashAlgo
lookupElementId 0x1C53BB6B = EE_Cues
lookupElementId 0x000000BB = EE_CuePoint
lookupElementId 0x000000B3 = EE_CueTime
lookupElementId 0x000000B7 = EE_CueTrackPositions
lookupElementId 0x000000F7 = EE_CueTrack
lookupElementId 0x000000F1 = EE_CueClusterPosition
lookupElementId 0x00005378 = EE_CueBlockNumber
lookupElementId 0x000000EA = EE_CueCodecState
lookupElementId 0x000000DB = EE_CueReference
lookupElementId 0x00000096 = EE_CueRefTime
lookupElementId 0x00000097 = EE_CueRefCluster
lookupElementId 0x0000535F = EE_CueRefNumber
lookupElementId 0x000000EB = EE_CueRefCodecState
lookupElementId 0x1941A469 = EE_Attachments
lookupElementId 0x000061A7 = EE_AttachedFile
lookupElementId 0x0000467E = EE_FileDescription
lookupElementId 0x0000466E = EE_FileName
lookupElementId 0x00004660 = EE_FileMimeType
lookupElementId 0x0000465C = EE_FileData
lookupElementId 0x000046AE = EE_FileUID
lookupElementId 0x00004675 = EE_FileReferral
lookupElementId 0x00004661 = EE_FileUsedStartTime
lookupElementId 0x00004662 = EE_FileUsedEndTime
lookupElementId 0x1043A770 = EE_Chapters
lookupElementId 0x000045B9 = EE_EditionEntry
lookupElementId 0x000045BC = EE_EditionUID
lookupElementId 0x000045BD = EE_EditionFlagHidden
lookupElementId 0x000045DB = EE_EditionFlagDefault
lookupElementId 0x000045DD = EE_EditionFlagOrdered
lookupElementId 0x000000B6 = EE_ChapterAtom
lookupElementId 0x000073C4 = EE_ChapterUID
lookupElementId 0x00000091 = EE_ChapterTimeStart
lookupElementId 0x00000092 = EE_ChapterTimeEnd
lookupElementId 0x00000098 = EE_ChapterFlagHidden
lookupElementId 0x00004598 = EE_ChapterFlagEnabled
lookupElementId 0x00006E67 = EE_ChapterSegmentUID
lookupElementId 0x00006EBC = EE_ChapterSegmentEditionUID
lookupElementId 0x000063C3 = EE_ChapterPhysicalEquiv
lookupElementId 0x0000008F = EE_ChapterTrack
lookupElementId 0x00000089 = EE_ChapterTrackNumber
lookupElementId 0x00000080 = EE_ChapterDisplay
lookupElementId 0x00000085 = EE_ChapString
lookupElementId 0x0000437C = EE_ChapLanguage
lookupElementId 0x0000437E = EE_ChapCountry
lookupElementId 0x00006944 = EE_ChapProcess
lookupElementId 0x00006955 = EE_ChapProcessCodecID
lookupElementId 0x0000450D = EE_ChapProcessPrivate
lookupElementId 0x00006911 = EE_ChapProcessCommand
lookupElementId 0x00006922 = EE_ChapProcessTime
lookupElementId 0x00006933 = EE_ChapProcessData
lookupElementId 0x1254C367 = EE_Tags
lookupElementId 0x00007373 = EE_Tag
lookupElementId 0x000063C0 = EE_Targets
lookupElementId 0x000068CA = EE_TargetTypeValue
lookupElementId 0x000063CA = EE_TargetType
lookupElementId 0x000063C5 = EE_TagTrackUID
lookupElementId 0x000063C9 = EE_TagEditionUID
lookupElementId 0x000063C4 = EE_TagChapterUID
lookupElementId 0x000063C6 = EE_TagAttachmentUID
lookupElementId 0x000067C8 = EE_SimpleTag
lookupElementId 0x000045A3 = EE_TagName
lookupElementId 0x0000447A = EE_TagLanguage
lookupElementId 0x00004484 = EE_TagDefault
lookupElementId 0x00004487 = EE_TagString
lookupElementId 0x00004485 = EE_TagBinary
lookupElementId id_ = EE_Unknown id_

lookupElementType EE_EBML                        = ET_Master
lookupElementType EE_EBMLVersion                 = ET_Unsigned
lookupElementType EE_EBMLReadVersion             = ET_Unsigned
lookupElementType EE_EBMLMaxIDLength             = ET_Unsigned
lookupElementType EE_EBMLMaxSizeLength           = ET_Unsigned
lookupElementType EE_DocType                     = ET_TextAscii
lookupElementType EE_DocTypeVersion              = ET_Unsigned
lookupElementType EE_DocTypeReadVersion          = ET_Unsigned
lookupElementType EE_Void                        = ET_Binary
lookupElementType EE_CRC32                       = ET_Binary
lookupElementType EE_SignatureSlot               = ET_Master
lookupElementType EE_SignatureAlgo               = ET_Unsigned
lookupElementType EE_SignatureHash               = ET_Unsigned
lookupElementType EE_SignaturePublicKey          = ET_Binary
lookupElementType EE_Signature                   = ET_Binary
lookupElementType EE_SignatureElements           = ET_Master
lookupElementType EE_SignatureElementList        = ET_Master
lookupElementType EE_SignedElement               = ET_Binary
lookupElementType EE_Segment                     = ET_Flatten
lookupElementType EE_SeekHead                    = ET_Master
lookupElementType EE_Seek                        = ET_Master
lookupElementType EE_SeekID                      = ET_Binary
lookupElementType EE_SeekPosition                = ET_Unsigned
lookupElementType EE_Info                        = ET_Master
lookupElementType EE_SegmentUID                  = ET_Binary
lookupElementType EE_SegmentFilename             = ET_TextUtf8
lookupElementType EE_PrevUID                     = ET_Binary
lookupElementType EE_PrevFilename                = ET_TextUtf8
lookupElementType EE_NextUID                     = ET_Binary
lookupElementType EE_NextFilename                = ET_TextUtf8
lookupElementType EE_SegmentFamily               = ET_Binary
lookupElementType EE_ChapterTranslate            = ET_Master
lookupElementType EE_ChapterTranslateEditionUID  = ET_Unsigned
lookupElementType EE_ChapterTranslateCodec       = ET_Unsigned
lookupElementType EE_ChapterTranslateID          = ET_Binary
lookupElementType EE_TimecodeScale               = ET_Unsigned
lookupElementType EE_Duration                    = ET_Float
lookupElementType EE_DateUTC                     = ET_Date
lookupElementType EE_Title                       = ET_TextUtf8
lookupElementType EE_MuxingApp                   = ET_TextUtf8
lookupElementType EE_WritingApp                  = ET_TextUtf8
lookupElementType EE_Cluster                     = ET_Flatten
lookupElementType EE_Timecode                    = ET_Unsigned
lookupElementType EE_SilentTracks                = ET_Master
lookupElementType EE_SilentTrackNumber           = ET_Unsigned
lookupElementType EE_Position                    = ET_Unsigned
lookupElementType EE_PrevSize                    = ET_Unsigned
lookupElementType EE_SimpleBlock                 = ET_Binary
lookupElementType EE_BlockGroup                  = ET_Master
lookupElementType EE_Block                       = ET_Binary
lookupElementType EE_BlockVirtual                = ET_Binary
lookupElementType EE_BlockAdditions              = ET_Master
lookupElementType EE_BlockMore                   = ET_Master
lookupElementType EE_BlockAddID                  = ET_Unsigned
lookupElementType EE_BlockAdditional             = ET_Binary
lookupElementType EE_BlockDuration               = ET_Unsigned
lookupElementType EE_ReferencePriority           = ET_Unsigned
lookupElementType EE_ReferenceBlock              = ET_Signed
lookupElementType EE_ReferenceVirtual            = ET_Signed
lookupElementType EE_CodecState                  = ET_Binary
lookupElementType EE_Slices                      = ET_Master
lookupElementType EE_TimeSlice                   = ET_Master
lookupElementType EE_LaceNumber                  = ET_Unsigned
lookupElementType EE_FrameNumber                 = ET_Unsigned
lookupElementType EE_BlockAdditionID             = ET_Unsigned
lookupElementType EE_Delay                       = ET_Unsigned
lookupElementType EE_SliceDuration               = ET_Unsigned
lookupElementType EE_ReferenceFrame              = ET_Master
lookupElementType EE_ReferenceOffset             = ET_Unsigned
lookupElementType EE_ReferenceTimeCode           = ET_Unsigned
lookupElementType EE_EncryptedBlock              = ET_Binary
lookupElementType EE_Tracks                      = ET_Master
lookupElementType EE_TrackEntry                  = ET_Master
lookupElementType EE_TrackNumber                 = ET_Unsigned
lookupElementType EE_TrackUID                    = ET_Unsigned
lookupElementType EE_TrackType                   = ET_Unsigned
lookupElementType EE_FlagEnabled                 = ET_Unsigned
lookupElementType EE_FlagDefault                 = ET_Unsigned
lookupElementType EE_FlagForced                  = ET_Unsigned
lookupElementType EE_FlagLacing                  = ET_Unsigned
lookupElementType EE_MinCache                    = ET_Unsigned
lookupElementType EE_MaxCache                    = ET_Unsigned
lookupElementType EE_DefaultDuration             = ET_Unsigned
lookupElementType EE_TrackTimecodeScale          = ET_Float
lookupElementType EE_TrackOffset                 = ET_Signed
lookupElementType EE_MaxBlockAdditionID          = ET_Unsigned
lookupElementType EE_Name                        = ET_TextUtf8
lookupElementType EE_Language                    = ET_TextAscii
lookupElementType EE_CodecID                     = ET_TextAscii
lookupElementType EE_CodecPrivate                = ET_Binary
lookupElementType EE_CodecName                   = ET_TextUtf8
lookupElementType EE_AttachmentLink              = ET_Unsigned
lookupElementType EE_CodecSettings               = ET_TextUtf8
lookupElementType EE_CodecInfoURL                = ET_TextAscii
lookupElementType EE_CodecDownloadURL            = ET_TextAscii
lookupElementType EE_CodecDecodeAll              = ET_Unsigned
lookupElementType EE_TrackOverlay                = ET_Unsigned
lookupElementType EE_TrackTranslate              = ET_Master
lookupElementType EE_TrackTranslateEditionUID    = ET_Unsigned
lookupElementType EE_TrackTranslateCodec         = ET_Unsigned
lookupElementType EE_TrackTranslateTrackID       = ET_Binary
lookupElementType EE_Video                       = ET_Master
lookupElementType EE_FlagInterlaced              = ET_Unsigned
lookupElementType EE_StereoMode                  = ET_Unsigned
lookupElementType EE_OldStereoMode               = ET_Unsigned
lookupElementType EE_PixelWidth                  = ET_Unsigned
lookupElementType EE_PixelHeight                 = ET_Unsigned
lookupElementType EE_PixelCropBottom             = ET_Unsigned
lookupElementType EE_PixelCropTop                = ET_Unsigned
lookupElementType EE_PixelCropLeft               = ET_Unsigned
lookupElementType EE_PixelCropRight              = ET_Unsigned
lookupElementType EE_DisplayWidth                = ET_Unsigned
lookupElementType EE_DisplayHeight               = ET_Unsigned
lookupElementType EE_DisplayUnit                 = ET_Unsigned
lookupElementType EE_AspectRatioType             = ET_Unsigned
lookupElementType EE_ColourSpace                 = ET_Binary
lookupElementType EE_GammaValue                  = ET_Float
lookupElementType EE_FrameRate                   = ET_Float
lookupElementType EE_Audio                       = ET_Master
lookupElementType EE_SamplingFrequency           = ET_Float
lookupElementType EE_OutputSamplingFrequency     = ET_Float
lookupElementType EE_Channels                    = ET_Unsigned
lookupElementType EE_ChannelPositions            = ET_Binary
lookupElementType EE_BitDepth                    = ET_Unsigned
lookupElementType EE_TrackOperation              = ET_Master
lookupElementType EE_TrackCombinePlanes          = ET_Master
lookupElementType EE_TrackPlane                  = ET_Master
lookupElementType EE_TrackPlaneUID               = ET_Unsigned
lookupElementType EE_TrackPlaneType              = ET_Unsigned
lookupElementType EE_TrackJoinBlocks             = ET_Master
lookupElementType EE_TrackJoinUID                = ET_Unsigned
lookupElementType EE_TrickTrackUID               = ET_Unsigned
lookupElementType EE_TrickTrackSegmentUID        = ET_Binary
lookupElementType EE_TrickTrackFlag              = ET_Unsigned
lookupElementType EE_TrickMasterTrackUID         = ET_Unsigned
lookupElementType EE_TrickMasterTrackSegmentUID  = ET_Binary
lookupElementType EE_ContentEncodings            = ET_Master
lookupElementType EE_ContentEncoding             = ET_Master
lookupElementType EE_ContentEncodingOrder        = ET_Unsigned
lookupElementType EE_ContentEncodingScope        = ET_Unsigned
lookupElementType EE_ContentEncodingType         = ET_Unsigned
lookupElementType EE_ContentCompression          = ET_Master
lookupElementType EE_ContentCompAlgo             = ET_Unsigned
lookupElementType EE_ContentCompSettings         = ET_Binary
lookupElementType EE_ContentEncryption           = ET_Master
lookupElementType EE_ContentEncAlgo              = ET_Unsigned
lookupElementType EE_ContentEncKeyID             = ET_Binary
lookupElementType EE_ContentSignature            = ET_Binary
lookupElementType EE_ContentSigKeyID             = ET_Binary
lookupElementType EE_ContentSigAlgo              = ET_Unsigned
lookupElementType EE_ContentSigHashAlgo          = ET_Unsigned
lookupElementType EE_Cues                        = ET_Master
lookupElementType EE_CuePoint                    = ET_Master
lookupElementType EE_CueTime                     = ET_Unsigned
lookupElementType EE_CueTrackPositions           = ET_Master
lookupElementType EE_CueTrack                    = ET_Unsigned
lookupElementType EE_CueClusterPosition          = ET_Unsigned
lookupElementType EE_CueBlockNumber              = ET_Unsigned
lookupElementType EE_CueCodecState               = ET_Unsigned
lookupElementType EE_CueReference                = ET_Master
lookupElementType EE_CueRefTime                  = ET_Unsigned
lookupElementType EE_CueRefCluster               = ET_Unsigned
lookupElementType EE_CueRefNumber                = ET_Unsigned
lookupElementType EE_CueRefCodecState            = ET_Unsigned
lookupElementType EE_Attachments                 = ET_Master
lookupElementType EE_AttachedFile                = ET_Master
lookupElementType EE_FileDescription             = ET_TextUtf8
lookupElementType EE_FileName                    = ET_TextUtf8
lookupElementType EE_FileMimeType                = ET_TextAscii
lookupElementType EE_FileData                    = ET_Binary
lookupElementType EE_FileUID                     = ET_Unsigned
lookupElementType EE_FileReferral                = ET_Binary
lookupElementType EE_FileUsedStartTime           = ET_Unsigned
lookupElementType EE_FileUsedEndTime             = ET_Unsigned
lookupElementType EE_Chapters                    = ET_Master
lookupElementType EE_EditionEntry                = ET_Master
lookupElementType EE_EditionUID                  = ET_Unsigned
lookupElementType EE_EditionFlagHidden           = ET_Unsigned
lookupElementType EE_EditionFlagDefault          = ET_Unsigned
lookupElementType EE_EditionFlagOrdered          = ET_Unsigned
lookupElementType EE_ChapterAtom                 = ET_Master
lookupElementType EE_ChapterUID                  = ET_Unsigned
lookupElementType EE_ChapterTimeStart            = ET_Unsigned
lookupElementType EE_ChapterTimeEnd              = ET_Unsigned
lookupElementType EE_ChapterFlagHidden           = ET_Unsigned
lookupElementType EE_ChapterFlagEnabled          = ET_Unsigned
lookupElementType EE_ChapterSegmentUID           = ET_Binary
lookupElementType EE_ChapterSegmentEditionUID    = ET_Unsigned
lookupElementType EE_ChapterPhysicalEquiv        = ET_Unsigned
lookupElementType EE_ChapterTrack                = ET_Master
lookupElementType EE_ChapterTrackNumber          = ET_Unsigned
lookupElementType EE_ChapterDisplay              = ET_Master
lookupElementType EE_ChapString                  = ET_TextUtf8
lookupElementType EE_ChapLanguage                = ET_TextAscii
lookupElementType EE_ChapCountry                 = ET_TextAscii
lookupElementType EE_ChapProcess                 = ET_Master
lookupElementType EE_ChapProcessCodecID          = ET_Unsigned
lookupElementType EE_ChapProcessPrivate          = ET_Binary
lookupElementType EE_ChapProcessCommand          = ET_Master
lookupElementType EE_ChapProcessTime             = ET_Unsigned
lookupElementType EE_ChapProcessData             = ET_Binary
lookupElementType EE_Tags                        = ET_Master
lookupElementType EE_Tag                         = ET_Master
lookupElementType EE_Targets                     = ET_Master
lookupElementType EE_TargetTypeValue             = ET_Unsigned
lookupElementType EE_TargetType                  = ET_TextAscii
lookupElementType EE_TagTrackUID                 = ET_Unsigned
lookupElementType EE_TagEditionUID               = ET_Unsigned
lookupElementType EE_TagChapterUID               = ET_Unsigned
lookupElementType EE_TagAttachmentUID            = ET_Unsigned
lookupElementType EE_SimpleTag                   = ET_Master
lookupElementType EE_TagName                     = ET_TextUtf8
lookupElementType EE_TagLanguage                 = ET_TextAscii
lookupElementType EE_TagDefault                  = ET_Unsigned
lookupElementType EE_TagString                   = ET_TextUtf8
lookupElementType EE_TagBinary                   = ET_Binary
lookupElementType (EE_Unknown _)                 = ET_Unknown
 
