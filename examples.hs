module Main where

import qualified ExampleUdpRecv
import qualified ExampleUdpSend
import qualified ExampleTransmux
import qualified ExamplePrint

import Text.Printf
import System.Environment (getArgs)

main = do
    args <- getArgs
    if length args < 1
        then do
            printf "Usage: examples {udp_send|udp_recv|transmux|print}\n"
            printf "   udp_send - read specified track from matroska file and send it as UDP datagrams\n"
            printf "   udp_recv - receive UDP messages and write them to matroska file\n"
            printf "   print    - read matroska file and print some information from it\n"
            printf "   transmux - read matroska file and write it\n"
        else let
            (head1:tail1) = args
            in
            case head1 of
                "udp_send" -> ExampleUdpSend.mainUdpSend tail1
                "udp_recv" -> ExampleUdpRecv.mainUdpRecv tail1
                "print" -> ExamplePrint.main
                "transmux" -> ExampleTransmux.main
