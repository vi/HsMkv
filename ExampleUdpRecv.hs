module ExampleUdpRecv where

-- Receive UDP datagrams and store them in matroska file
-- (with appropriate timecodes based of current UNIX time)

-- Example:
--   cvlc something.wav --sout '#transcode{acodec=mp3,ab=500}:standard{access=udp,mux=mp3,dst="127.0.0.1:5567"}'
--   example_udp_recv TTAudio A_MPEG/L3 0.0.0.0 5567 > something.mkv

import Network.Socket
import qualified Network.Socket.ByteString as NB
import Network
import Network.BSD
import System.Environment (getArgs)
import Text.Printf
import System.IO
import qualified Codec.HsMkv.Model as M
import qualified Codec.HsMkv.MkvGen as M
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import Data.List
import Data.Maybe
import Data.Time.Clock.POSIX
import Control.Monad

main = do
    args <- getArgs
    mainUdpRecv args
mainUdpRecv args =
    if length args < 4
        then do
            printf "Usage: example_udp_recv type CodecID host port > matroska_file.mkv\n"
            printf "      types:  TTAudio TTVideo TTSubtitle TTComplex TTLogo TTButton TTControl\n"
        else withSocketsDo $ do
            let (arg1s:arg2s:arg3s:arg4s:_) = args
            let (track_type, codecID, hostname, port) = (
                    read arg1s :: M.TrackType, 
                    T.pack arg2s, 
                    arg3s,
                    read   arg4s
                    )
            sock <- socket AF_INET Datagram 0
            host <- getHostByName hostname
            let port' =  fromIntegral port :: PortNumber
            bindSocket sock (SockAddrInet port' $ hostAddress host)
 
            now <- liftM realToFrac getPOSIXTime

            B.hPut stdout M.matroskaHeader

            let outputEvent event = (
                  B.hPut stdout $ M.writeMatroskaElement $ 
                    fromJust $ M.eventToElement 1000000 event)

            outputEvent $ M.MEInfo M.blankInfo {
                 M.iWritingApplication = Just $ T.pack "HsMkv example_udp_recv"
                ,M.iDate = Just now
                }   
            outputEvent $ M.METracks [M.blankTrack {
                M.tNumber = 1
               ,M.tUID = Just 1
               ,M.tType = track_type
               ,M.tCodecId = codecID
               }]    

            let handleBuffer (buf, timestamp) = outputEvent $ M.MEFrame M.blankFrame {
                 M.fTrackNumber = 1
                ,M.fTimeCode = timestamp
                ,M.fData = [buf]
                }

            process handleBuffer sock
            where
            process handleBuffer sock = do
                buf <- NB.recv sock 102400
                now <- getPOSIXTime
                handleBuffer (B.fromChunks [buf], realToFrac now)
                process handleBuffer sock

