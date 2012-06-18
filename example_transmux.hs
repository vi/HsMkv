-- Read mkv from stdin, demux, mux and write to stdout

module Main where

import qualified MkvParse as M
import qualified MkvGen as MG
import qualified Data.ByteString.Lazy as B
import System.IO

main :: IO ()
main = do
    contents <- B.hGetContents System.IO.stdin
    mkvevents <- return $ M.parseMkv contents
    B.hPut System.IO.stdout $ MG.writeMkv mkvevents
