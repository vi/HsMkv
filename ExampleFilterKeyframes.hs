-- Read mkv from stdin, demux, remove all non-key frames, mux and write to stdout

module ExampleFilterKeyframes where

import qualified Codec.HsMkv.MkvParse as M
import qualified Codec.HsMkv.Model as M
import qualified Codec.HsMkv.MkvGen as MG
import qualified Data.ByteString.Lazy as B
import System.IO
import qualified Data.Maybe as DM

keyframesFilter :: M.MatroskaEvent -> Maybe M.MatroskaEvent
keyframesFilter (M.MEFrame x) | not $ M.fKeyframe x  = Nothing
keyframesFilter x = Just x

main :: IO ()
main = do
    contents <- B.hGetContents System.IO.stdin
    let mkvevents = M.parseMkv contents
    let filtered = DM.mapMaybe keyframesFilter mkvevents
    B.hPut System.IO.stdout $ MG.writeMkv filtered
