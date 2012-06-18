-- Read mkv from stdin, demux, mux and write to stdout

module Main where

import qualified Codec.HsMkv.MkvParse as M
import qualified Codec.HsMkv.MkvGen as MG
import qualified Data.ByteString.Lazy as B
import System.IO

main :: IO ()
main = do
    contents <- B.hGetContents System.IO.stdin
    mkvevents <- return $ M.parseMkv contents
    B.hPut System.IO.stdout $ MG.writeMkv mkvevents
