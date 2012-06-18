Matroska demuxer and muxer in Haskell. 
Feed the content of mkv file to MkvParse.parseMkv and get list of frames (and other events)
The same list of events can be conveted back to ByteString using MkvGen.writeMkv

    parseMkv :: Data.ByteString.Lazy.ByteString -> [MatroskaEvent]

    data MatroskaEvent = ME_Tracks ... | ME_Info .. | ME_Frame Frame | ...

    data Frame = Frame {
        f_trackNumber :: Integer,
        f_timeCode :: Double,
        f_data :: [B.ByteString],  -- laced sub-frames
        f_duration :: Maybe Double,
        f_invisible :: Bool,
        f_discardable :: Bool,
        f_keyframe :: Bool
    } deriving (Show)
    
    -- See example_print.hs for more complete example

Supported features:

*  decoding of SimpleBlocks and BlockGroups
*  handling TimecodeScale
*  lacing
*  resyncing after errors
*  muxing (see example_transmux.hs)

Not supported features:

*  track timecode scale multiplier
*  seeking, cues, chapters


Python version of demuxer and Matroska XML tools: https://github.com/vi/mkvparse

License=MIT, Vitaly "_Vi" Shukela, 2012.

