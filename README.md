Matroska demuxer in Haskell. Feed it content of mkv file and get list of frames (and other events)

    parseMkv :: Data.ByteString.Lazy.ByteString -> [MatroskaEvent]

    data MatroskaEvent = ME_Tracks ... | ME_Info .. | ME_Frame MatroskaFrame | ...

    data Frame = Frame {
        f_trackNumber :: Integer,
        f_timeCode :: Double,
        f_data :: [B.ByteString],  -- laced sub-frames
        f_duration :: Maybe Double
    } deriving (Show)

Supported features:

*  decoding of SimpleBlocks and BlockGroups
*  handling TimecodeScale
*  lacing
*  resyncing after errors

Not supported features:

*  track timecode scale multiplier
*  seeking, cues, chapters


Python version: https://github.com/vi/mkvparse

License=MIT, Vitaly "_Vi" Shukela, 2012.

