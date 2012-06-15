module Main where

import qualified MkvParse as M

import qualified Data.ByteString.Lazy as B
import System.IO
import Text.Printf
import qualified Data.Text as T
import Data.Char
import Debug.Trace


-- toHex :: B.ByteString -> String
-- toHex q = concat $ (printf "%02x" :: Int -> String) `fmap` (fromIntegral `fmap` B.unpack q)


demoHandler :: M.MatroskaEvent -> IO ()
demoHandler M.ME_Resync = printf "Resync\n"
--demoHandler (M.ME_EbmlElement (M.MatroskaElement klass size content)) = printf " %s %d\n" (show klass) size

demoHandler (M.ME_Info inf) =  do
    printf "Info\n"
    printf "  Title: %s\n"               $ T.unpack $ M.i_title inf
    printf "  Timecode scale: %d\n"                 $ M.i_timecodeScale inf
    printf "  Muxing application: %s\n"  $ T.unpack $ M.i_muxingApplication inf
    printf "  Writing application: %s\n" $ T.unpack $ M.i_writingApplication inf
    printf "  Duration: %g\n"                       $ M.i_duration inf
    printf "  Segment UID: %s\n"         $ T.unpack $ M.i_segmentUid inf

demoHandler (M.ME_Tracks tracks) = mapM_ ttt tracks
    where
    ttt :: M.Track -> IO()
    ttt t = do
        printf "Track %d\n"                    $ M.t_number t
        printf "  Type: %s\n"      $ show      $ M.t_type t
        printf "  CodecID: %s\n"   $ T.unpack  $ M.t_codecId t
        printf "  Language: %s\n"  $ T.unpack  $ M.t_language t

demoHandler (M.ME_Frame frame) = do
    printf "Frame for %d ts=%.06f lace=%d %s len=%d data=%s...\n" tn ts lace dur len bufstr
        where
        tn   =           M.f_trackNumber frame
        ts   =           M.f_timeCode frame
        buf = B.concat $ M.f_data frame
        bufstr =  T.unpack $ M.toHex $ B.take 10 buf
        lace = length $  M.f_data frame
        len = B.length $ B.concat $ M.f_data frame
        dur = case M.f_duration frame of
            Nothing -> ""
            Just x -> printf "dur=%.06f" x

demoHandler _ = return ()



main :: IO ()
main = do
    contents <- B.hGetContents System.IO.stdin
    mkvevents <- return $ M.parseMkv contents
    mapM_ demoHandler mkvevents

-- s2b x = B.pack $ map (fromIntegral . ord) x