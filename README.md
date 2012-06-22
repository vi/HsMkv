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
    
    -- See ExamplePrint.hs for more complete example

Supported features:

*  decoding of SimpleBlocks and BlockGroups
*  handling TimecodeScale
*  lacing
*  resyncing after errors
*  muxing

Not supported features:

*  track timecode scale multiplier
*  seeking, cues, chapters
*  ContentEncoding
*  Access to various parameters of Track or Info (can be easily added probably)

Included examples:

* ExamplePrint - read matroska file and print info, tracks and frames
* ExampleTransmux - read matroska file and write it
* ExampleUdpRecv - receive UDP packets and write them as frames to matroska file (with timecodes), like in [udp2mkv](http://vi-server.org/pub/mkv2udp.c)
* ExampleUdpSend - read frames from matroska file and send it as UDP packets, honouring timecodes, like in [mkv2udp](http://vi-server.org/pub/udp2mkv.c)
* examples.hs - bundle all example above in one executable

There are compiled versions "examples.hs" for [linux](http://vi-server.org/pub/HsMkv) and [windows](http://vi-server.org/pub/HsMkv.exe).


Python version of demuxer and Matroska XML tools: https://github.com/vi/mkvparse

License=MIT, Vitaly "_Vi" Shukela, 2012.

