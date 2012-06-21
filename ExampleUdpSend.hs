module ExampleUdpSend where

-- Read matroska file and send contents of specified track to udp://host:port
-- (with delays according to timecodes)
-- Example:
--   example_udp_send 1 127.0.0.1 5567 < something.mkv
--   cvlc udp://@:5567

import Data.List
import Data.Maybe
import Control.Monad
import qualified Codec.HsMkv.MkvParse as M
import qualified Codec.HsMkv.Model as M
import Network.Socket
import Network
import Network.BSD
import qualified Network.Socket.ByteString as NB
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as BN
import qualified Data.ByteString.Lazy.Char8 as C
import System.Environment (getArgs)
import Text.Printf
import System.IO
import Control.Concurrent (threadDelay) 
import Data.Time.Clock.POSIX

convertByteString = BN.concat . B.toChunks

udpSendHandlerHandler :: Double -> Integer -> Socket -> SockAddr -> (Maybe Double, [M.MatroskaEvent]) -> Maybe (IO (), (Maybe Double, [M.MatroskaEvent]))
udpSendHandlerHandler initial_time track sock addr = h
    where
    h (tb, M.MEResync:tail1) = Just (printf "Resync\n", (tb, tail1))
    h (tb, M.MEFrame f : tail1) = Just (event, (Just timebase, tail1))
        where
        event = if M.f_trackNumber f == track
                then do
                    now <- getPOSIXTime
                    let now' = realToFrac now
                    let how_much_to_sleep = floor $ 1000000*(ts_of_this_frame + timebase - now')
                    let how_much_to_sleep' = if how_much_to_sleep < 0 then 0 else how_much_to_sleep
                    printf "%d" $ length $ M.f_data f
                    hFlush stdout
                    threadDelay how_much_to_sleep'
                    NB.sendTo sock (convertByteString $ B.concat $ M.f_data f) addr
                    return ()
                else do
                    printf "."
                    hFlush stdout
        timebase = fromMaybe (initial_time - ts_of_this_frame) tb
        ts_of_this_frame  = M.f_timeCode f
    h (tb, M.METracks tracks : tail1) = Just (event, (tb, tail1))
        where
        event = case find (\x -> M.t_number x == track) tracks of
            Nothing -> return ()  -- our track is not found
            Just my_track -> case M.t_codecPrivate my_track of
                Nothing -> return () -- no "CodecPrivate" for our track
                Just x' -> void $ NB.sendTo sock (convertByteString x') addr
    h (tb, _:tail1) = Just (return (), (tb, tail1))
    h (tb, []) = Nothing
            


main = do
    args <- getArgs
    mainUdpSend args
mainUdpSend args =
    if length args < 3 
        then printf "Usage: example_udp_send track_number host port < matroska_file.mkv\n"
        else withSocketsDo $ do
            let (track_s: hostname_s: port_s:tail1) = args
            let (track, hostname, port) = (read track_s :: Integer, hostname_s, read port_s)
            sock <- socket AF_INET Datagram 0
            host <- getHostByName hostname
            let port' =  fromIntegral port :: PortNumber
            let addr = SockAddrInet port' $ hostAddress host 

            contents <- B.hGetContents System.IO.stdin
            let mkvevents = M.parseMkv contents
            initial_time <- getPOSIXTime
            let initial_time' = realToFrac initial_time
            let actions = unfoldr (udpSendHandlerHandler initial_time' track sock addr) (Nothing, mkvevents)
            sequence_ actions
            printf "\n"
