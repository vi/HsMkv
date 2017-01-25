-- Read mkv from stdin, demux, mux and write to stdout

module ExamplePtsFromFile where

import qualified Codec.HsMkv.Model as M
import qualified Codec.HsMkv.MkvParse as M
import qualified Codec.HsMkv.MkvGen as MG
import qualified Data.ByteString.Lazy as B
import System.Environment (getArgs)
import Text.Printf
import System.IO
import qualified System.IO.Unsafe as U

setpts :: Handle -> M.MatroskaEvent -> M.MatroskaEvent
setpts fh (M.MEFrame frame) = do
    let pts = U.unsafePerformIO $ hGetLine fh
    M.MEFrame frame{M.fTimeCode = read pts}
setpts _ x = x

main :: IO ()
main = do
    args <- getArgs
    mainPtsFromFile args

mainPtsFromFile args = do
    if length args /= 1
        then do
            printf "Usage: example_setpts pts.txt < matroska_in.mkv > matroska_out.mkv\n"
        else do
            let (arg1s:_) = args
            fh <- openFile arg1s ReadMode
            contents <- B.hGetContents System.IO.stdin
            let mkvevents = M.parseMkv contents
            let mkveventsout = map (setpts fh) mkvevents
            B.hPut System.IO.stdout $ MG.writeMkv mkveventsout
