module ExamplePrint where

import qualified Codec.HsMkv.MkvParse as M
import qualified Codec.HsMkv.Model as M

import qualified Data.ByteString.Lazy as B
import System.IO
import Text.Printf
import qualified Data.Text as T
import Control.Monad 
import Data.Char
import Debug.Trace


-- toHex :: B.ByteString -> String
-- toHex q = concat $ (printf "%02x" :: Int -> String) `fmap` (fromIntegral `fmap` B.unpack q)


demoHandler :: M.MatroskaEvent -> IO ()
demoHandler M.MEResync = printf "Resync\n"
--demoHandler (M.MEEbmlElement (M.MatroskaElement klass (Just size) content)) = printf " %s %d\n" (show klass) size


demoHandler (M.MEInfo inf) = let
    printf_m :: (PrintfArg a) => String -> Maybe a -> IO ()
    printf_m fmt = maybe (return ()) (printf fmt)
    in do
        printf   "Info\n"
        printf_m "  Title: %s\n"               $ liftM T.unpack $ M.iTitle inf
        printf   "  Timecode scale: %d\n"                       $ M.iTimecodeScale inf
        printf_m "  Muxing application: %s\n"  $ liftM T.unpack $ M.iMuxingApplication inf
        printf_m "  Writing application: %s\n" $ liftM T.unpack $ M.iWritingApplication inf
        printf_m "  Duration: %g\n"                             $ M.iDuration inf
        printf_m "  Segment UID: %s\n"         $ liftM T.unpack $ M.iSegmentUid inf

demoHandler (M.METracks tracks) = mapM_ ttt tracks
    where
    ttt :: M.Track -> IO()
    ttt t = do
        printf "Track %d\n"                    $ M.tNumber t
        printf "  Type: %s\n"      $ show      $ M.tType t
        printf "  CodecID: %s\n"   $ T.unpack  $ M.tCodecId t
        case M.tLanguage t of
            Just x -> printf "  Language: %s\n"  $ T.unpack x
            Nothing -> return ()

demoHandler (M.MEFrame frame) =
    printf "Frame for %d %s%s%s ts=%.06f lace=%d %s len=%d data=%s...\n" tn fI fD fK ts lace dur len bufstr
        where
        tn   =           M.fTrackNumber frame
        ts   =           M.fTimeCode frame
        buf = B.concat $ M.fData frame
        bufstr =  T.unpack $ M.toHex $ B.take 10 buf
        lace = length $  M.fData frame
        len = B.length $ B.concat $ M.fData frame
        dur = case M.fDuration frame of
            Nothing -> ""
            Just x -> printf "dur=%.06f" x
        fI = if M.fInvisible   frame then "i" else " "
        fD = if M.fDiscardable frame then "D" else " "
        fK = if M.fKeyframe    frame then "K" else " "

demoHandler _ = return ()



main :: IO ()
main = do
    contents <- B.hGetContents System.IO.stdin
    let mkvevents = M.parseMkv contents
    mapM_ demoHandler mkvevents

-- s2b x = B.pack $ map (fromIntegral . ord) x
