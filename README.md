Matroska demuxer and muxer in Haskell. 
Feed the content of mkv file to MkvParse.parseMkv and get list of frames (and other events)
The same list of events can be conveted back to ByteString using MkvGen.writeMkv

    parseMkv :: Data.ByteString.Lazy.ByteString -> [MatroskaEvent]

    data MatroskaEvent = ME_Tracks ... | ME_Info .. | ME_Frame Frame | ...

    data Frame = Frame {
        fTrackNumber :: Integer,
        fTimeCode :: Double,
        fData :: [B.ByteString],  -- laced subframes
        fDuration :: Maybe Double,
        fInvisible :: Bool,
        fDiscardable :: Bool,
        fKeyframe :: Bool
    } deriving (Show)
    
    -- See ExamplePrint.hs for more complete example

Supported features:

*  decoding of SimpleBlocks and BlockGroups
*  handling TimecodeScale
*  lacing
*  resyncing after errors
*  muxing

Not supported features:

*  track timecode scale multiplier
*  seeking (with index), cues, chapters
*  ContentEncoding (except of "header removal")
*  Access to various parameters of Track or Info (can be easily added probably)

Included examples:

* ExamplePrint - read matroska file and print info, tracks and frames
* ExampleTransmux - read matroska file and write it
* ExampleUdpRecv - receive UDP packets and write them as frames to matroska file (with timecodes), like in [udp2mkv](http://vi-server.org/pub/udp2mkv.c)
* ExampleUdpSend - read frames from matroska file and send it as UDP packets, honouring timecodes, like in [mkv2udp](http://vi-server.org/pub/mkv2udp.c)
* examples.hs - bundle all example above in one executable

There are compiled versions "examples.hs" for [linux](http://vi-server.org/pub/HsMkv) and [windows](http://vi-server.org/pub/HsMkv.exe).


Python version of demuxer and Matroska XML tools: https://github.com/vi/mkvparse

License=MIT, Vitaly "_Vi" Shukela, 2012.

