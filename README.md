Matroska demuxer in Haskell. Feed it content of mkv file and get list of frames

    parseMkv :: Data.ByteString.Lazy.ByteString -> [MatroskaEvent]

    data MatroskaEvent = TracksAvailable ... | SegmentInfoAvailable .. | FrameReceived MatroskaFrame | ...

    data MatroskaFrame = MatroskaFrame {
        trackNumber :: TrackNumber,
        timeCode :: TimeCode,
        frameData :: B.ByteString,
        moreLacedFrames :: Int,
        duration :: Maybe Duration
    } deriving (Show)

Supported features:

*  decoding of SimpleBlocks and BlockGroups
*  lacing

Not supported features:

*  timecode scale is hard coded to 1000000 (TODO)
*  user-friendly format for Tracks and SegmentInfo (TODO)
*  track timecode scale multiplier
*  seeking, cues, chapters
*  resyncing after errors

*  the processing speed seems to be even slower than in Python mkvparse

Python version: https://github.com/vi/mkvparse

License=MIT, Vitaly "_Vi" Shukela, 2012.

