module Main where

import qualified ExampleUdpRecv
import qualified ExampleUdpSend
import qualified ExampleTransmux
import qualified ExampleFilterKeyframes
import qualified ExamplePrint
import qualified ExampleSetpts
import qualified ExampleDump

import Text.Printf
import System.Environment (getArgs)

main = do
    args <- getArgs
    if length args < 1
        then do
            printf "Usage: examples {udp_send|udp_recv|transmux|print|setpts}\n"
            printf "   udp_send - read specified track from matroska file and send it as UDP datagrams\n"
            printf "   udp_recv - receive UDP messages and write them to matroska file\n"
            printf "   print    - read matroska file and print some information from it\n"
            printf "   transmux - read matroska file and write it\n"
            printf "   keysonly - read matroska file and write only key frames\n"
            printf "   setpts   - like transmux, but also fiddle with timecodes\n"
            printf "   dump     - dump binary content of all frames to stdout\n"
        else let
            (head1:tail1) = args
            in
            case head1 of
                "udp_send" -> ExampleUdpSend.mainUdpSend tail1
                "udp_recv" -> ExampleUdpRecv.mainUdpRecv tail1
                "print" -> ExamplePrint.main
                "transmux" -> ExampleTransmux.main
                "keysonly" -> ExampleFilterKeyframes.main
                "setpts" -> ExampleSetpts.mainSetpts tail1
                "dump" -> ExampleDump.main
