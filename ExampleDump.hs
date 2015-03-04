module ExampleDump where

import qualified Codec.HsMkv.MkvParse as M
import qualified Codec.HsMkv.Model as M

import qualified Data.ByteString.Lazy as B
import System.IO
import Control.Monad 
import Data.Char
import Debug.Trace


demoHandler (M.MEFrame frame) =
    B.hPut System.IO.stdout buf
        where
        buf = B.concat $ M.fData frame

demoHandler _ = return ()


main :: IO ()
main = do
    contents <- B.hGetContents System.IO.stdin
    let mkvevents = M.parseMkv contents
    mapM_ demoHandler mkvevents

-- s2b x = B.pack $ map (fromIntegral . ord) x
