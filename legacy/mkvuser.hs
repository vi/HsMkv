module Main where

import Mkvparse

import qualified Data.ByteString.Lazy as B
import System.IO
import Text.Printf
import Data.Char
import Debug.Trace


toHex :: B.ByteString -> String
toHex q = concat $ (printf "%02x" :: Int -> String) `fmap` (fromIntegral `fmap` B.unpack q)

eventHandler :: [MatroskaEvent] -> IO ()
eventHandler [] = return ()
eventHandler (TracksAvailable tracks:xs) = do
    mapM_ (putStr . printf "Track %s\n". show) tracks
    eventHandler xs
eventHandler (SegmentInfoAvailable infos:xs) = do
    mapM_ (putStr . printf "Segment info %s\n" . show) infos
    eventHandler xs
eventHandler (FrameReceived frame:xs) = do
    -- printf "Frame for %d ts=%.06f l=%d %s len=%s data=%s..." tn ts lace dur (B.length buf) bufstr
    printf "Frame for %d ts=%.06f l=%d %s len=%d data=%s...\n" tn ts lace dur (B.length buf) bufstr
    eventHandler xs
        where
        tn = trackNumber frame
        ts = timeCode frame
        buf = frameData frame
        bufstr =  toHex $ B.take 10 buf
        lace = moreLacedFrames frame
        dur = case duration frame of
            Nothing -> ""
            Just x -> printf "dur=%.06f" x
eventHandler (_:xs) = eventHandler xs
    
    


main :: IO ()
main = do
    contents <- B.hGetContents System.IO.stdin
    mkvevents <- return $ parseMkv contents
    eventHandler mkvevents

-- s2b x = B.pack $ map (fromIntegral . ord) x
