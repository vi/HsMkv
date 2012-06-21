module ExampleUdpRecv where

-- Receive UDP datagrams and store them in matroska file
-- (with appropriate timecodes based of current UNIX time)

-- Example:
--   cvlc something.wav --sout '#transcode{acodec=mp3,ab=500}:standard{access=udp,mux=mp3,dst="127.0.0.1:5567"}'
--   example_udp_recv TT_Audio A_MPEG/L3 0.0.0.0 5567 > something.mkv

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
            printf "      types:  TT_Audio TT_Video TT_Subtitle TT_Complex TT_Logo TT_Button TT_Control\n"
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

            outputEvent $ M.ME_Info M.blankInfo {
                 M.i_writingApplication = Just $ T.pack "HsMkv example_udp_recv"
                ,M.i_date = Just now
                }   
            outputEvent $ M.ME_Tracks [M.blankTrack {
                M.t_number = 1
               ,M.t_UID = Just 1
               ,M.t_type = track_type
               ,M.t_codecId = codecID
               }]    

            let handleBuffer (buf, timestamp) = outputEvent $ M.ME_Frame M.blankFrame {
                 M.f_trackNumber = 1
                ,M.f_timeCode = timestamp
                ,M.f_data = [buf]
                }

            process handleBuffer sock
            where
            process handleBuffer sock = do
                buf <- NB.recv sock 102400
                now <- getPOSIXTime
                handleBuffer (B.fromChunks [buf], realToFrac now)
                process handleBuffer sock

