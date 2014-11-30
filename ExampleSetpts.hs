-- Read mkv from stdin, demux, mux and write to stdout

module ExampleSetpts where

import qualified Codec.HsMkv.Model as M
import qualified Codec.HsMkv.MkvParse as M
import qualified Codec.HsMkv.MkvGen as MG
import qualified Data.ByteString.Lazy as B
import System.Environment (getArgs)
import Text.Printf
import System.IO

setpts :: Double -> Double -> M.MatroskaEvent -> M.MatroskaEvent
setpts coe off (M.MEFrame frame) = M.MEFrame frame{M.fTimeCode = off + coe * M.fTimeCode frame}
setpts _ _ x = x

main :: IO ()
main = do
    args <- getArgs
    mainSetpts args

mainSetpts args = do
    if length args < 2
        then do
            printf "Usage: example_setpts coefficient offset < matroska_in.mkv > matroska_out.mkv\n"
        else do
            let (arg1s:arg2s:_) = args
            let (coef, offs) = (
                    read arg1s :: Double,
                    read arg2s :: Double
                    )
            contents <- B.hGetContents System.IO.stdin
            let mkvevents = M.parseMkv contents
            let mkveventsout = map (setpts coef offs) mkvevents
            B.hPut System.IO.stdout $ MG.writeMkv mkveventsout
