module ExamplePrintpts where

import qualified Codec.HsMkv.Model as M
import qualified Codec.HsMkv.MkvParse as M
import qualified Codec.HsMkv.MkvGen as MG
import qualified Data.ByteString.Lazy as B
import System.Environment (getArgs)
import Text.Printf
import System.IO
import System.IO.Unsafe as U

dumppts :: Handle -> M.MatroskaEvent -> IO ()
dumppts h (M.MEFrame frame) = do
    hPutStrLn h $ show $ M.fTimeCode frame
dumppts h x = return ()

main :: IO ()
main = do
    args <- getArgs
    mainPrintpts args

mainPrintpts args = do
    if length args > 0
        then do
            printf "Usage: example_printpts < matroska_in.mkv > pts.txt\n"
        else do
            contents <- B.hGetContents System.IO.stdin
            let mkvevents = M.parseMkv contents
            mapM (dumppts System.IO.stdout) mkvevents
            return ()
