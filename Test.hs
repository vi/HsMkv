module Main where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Text as T

import Test.HUnit
import Codec.HsMkv.MkvParse
import Codec.HsMkv.MkvGen
import Codec.HsMkv.MkvTabular

-- getMajorBitTests = TestCase $ do
--    assertEqual "0"  Nothing  $ getMajorBit (0    :: Int) 
--    assertEqual "80" (Just 0) $ getMajorBit (0x80 :: Int)
--    assertEqual "40" (Just 1) $ getMajorBit (0x40 :: Int)
--    assertEqual "01" (Just 7) $ getMajorBit (0x01 :: Int)

readEbmlNumberTests = TestCase $ do
    assertEqual "null" Nothing                  $ (readEbmlNumber ENUnsigned   $ B.empty)
    assertEqual "x80"  (Just (0,B.empty))       $ (readEbmlNumber ENUnsigned   $ C.pack "\x80") 
    assertEqual "x81"  (Just (1,B.empty))       $ (readEbmlNumber ENUnsigned   $ C.pack "\x81") 
    assertEqual "post" (Just (1,C.pack "\x80")) $ (readEbmlNumber ENUnsigned   $ C.pack "\x81\x80") 
    assertEqual "nulL" Nothing                  $ (readEbmlNumber ENUnsigned   $ C.pack "\x40")
    assertEqual "x3F"  (Just (0x3FFE, B.empty)) $ (readEbmlNumber ENUnsigned   $ C.pack "\x7F\xFE")
    assertEqual "u3F"  (Just (0x7FFE, B.empty)) $ (readEbmlNumber ENUnmodified $ C.pack "\x7F\xFE")
    assertEqual "u10"  (Just (0x10000000, B.empty)) $ (readEbmlNumber ENUnmodified $ C.pack "\x10\x00\x00\x00")

readBigEndianNumberTests = TestCase $ do
    assertEqual "B0"  Nothing                   $ (readBigEndianNumber False $ C.pack "")
    assertEqual "B1"  (Just 0x44)               $ (readBigEndianNumber False $ C.pack "\x44")
    assertEqual "B2"  (Just 0x4444)             $ (readBigEndianNumber False $ C.pack "\x44\x44")
    assertEqual "B3"  (Just 0x444444)           $ (readBigEndianNumber False $ C.pack "\x44\x44\x44")
    assertEqual "B4"  (Just 0x44444444)         $ (readBigEndianNumber False $ C.pack "\x44\x44\x44\x44")
    assertEqual "S4"  (Just 0x44444444)         $ (readBigEndianNumber True  $ C.pack "\x44\x44\x44\x44")
    assertEqual "S5"  (Just (-0x44444444))      $ (readBigEndianNumber True  $ C.pack "\xBB\xBB\xBB\xBC")
    assertEqual "S6"  (Just 0xBBBBBBBC)         $ (readBigEndianNumber False $ C.pack "\xBB\xBB\xBB\xBC")

readXiphLacingNumberTests = TestCase $ do
    assertEqual "L0"  Nothing                   $ (readXiphLacingNumber $ C.pack "")
    assertEqual "L1"  (Just (4, C.pack ""))     $ (readXiphLacingNumber $ C.pack "\x04")
    assertEqual "L2"  (Just (4, C.pack "\x04")) $ (readXiphLacingNumber $ C.pack "\x04\x04")
    assertEqual "L3"  (Just (255+4, C.pack "")) $ (readXiphLacingNumber $ C.pack "\xFF\x04")
    assertEqual "L4"  Nothing                   $ (readXiphLacingNumber $ C.pack "\xFF")
    assertEqual "L5"  Nothing                   $ (readXiphLacingNumber $ C.pack "\xFF\xFF")
    assertEqual "L6"  (Just (514, C.pack ""))   $ (readXiphLacingNumber $ C.pack "\xFF\xFF\x04")


tryParseEbml1Tests = TestCase $ do
    assertEqual "MM0"
        (Just (MatroskaElement EE_MinCache (Just 2) (EC_Unsigned 0x1234), C.pack ";BC"))
        (tryParseEbml1 $ C.pack "\x6D\xE7\x82\x12\x34;BC")
    assertEqual "MM1"
        Nothing
        (tryParseEbml1 $ C.pack "\x6D\xE7\x80;BC")
    assertEqual "MM2"
        (Just (MatroskaElement EE_MinCache (Just 2) (EC_Unsigned 0x1234), C.pack ""))
        (tryParseEbml1 $ C.pack "\x6D\xE7\x82\x12\x34")
    assertEqual "MM3"
        (Just (MatroskaElement EE_TrackOffset (Just 1) (EC_Signed (-4)), C.pack ";BC"))
        (tryParseEbml1 $ C.pack "\x53\x7F\x81\xFC;BC")
    assertEqual "MMB"
        (Just (MatroskaElement EE_SegmentUID (Just 1) (EC_Binary (C.pack "H")), C.pack ";BC"))
        (tryParseEbml1 $ C.pack "\x73\xA4\x81H;BC")
    assertEqual "MMb"
        (Just (MatroskaElement EE_SegmentUID (Just 0) (EC_Binary (C.pack "")), C.pack "H;BC"))
        (tryParseEbml1 $ C.pack "\x73\xA4\x80H;BC")
    assertEqual "MM4"
        (Just (MatroskaElement EE_Title (Just 8) (EC_TextUtf8 (T.pack "тест")), C.pack ";BC"))
        (tryParseEbml1 $ C.pack "\x7B\xA9\x88\xd1\x82\xd0\xb5\xd1\x81\xd1\x82;BC")
    assertEqual "MM5"
        (Just (MatroskaElement EE_CodecID (Just 5) (EC_TextAscii (T.pack "A_AAC")), C.pack ";BC"))
        (tryParseEbml1 $ C.pack "\x86\x85\&A_AAC;BC")
    assertEqual "MM6"
        (Just (MatroskaElement EE_DateUTC (Just 8) (EC_Date 1340061045.0), C.pack ";BC"))
        (tryParseEbml1 $ C.pack "\x44\x61\x88\x05\x05\x3B\xC0\xF4\xCC\x92\x00;BC")
    assertEqual "MM7"
        (Just (MatroskaElement EE_SamplingFrequency (Just 8) (EC_Float 1234.5), C.pack ";BC"))
        (tryParseEbml1 $ C.pack "\xB5\x88\x40\x93\x4a\x00\x00\x00\x00\x00;BC")
    assertEqual "MM8"
        (Just (MatroskaElement EE_SamplingFrequency (Just 4) (EC_Float 1234.5), C.pack ";BC"))
        (tryParseEbml1 $ C.pack "\xB5\x84\x44\x9a\x50\x00;BC")
    assertEqual "MM9"
        (Just (MatroskaElement EE_Tracks (Just 4) (EC_Master [
             (MatroskaElement EE_TrackEntry (Just 0) (EC_Master []))
            ,(MatroskaElement EE_TrackEntry (Just 0) (EC_Master []))
            ]), C.pack ";BC"))
        (tryParseEbml1 $ C.pack "\x16\x54\xAE\x6B\x84\xAE\x80\xAE\x80;BC")

resyncTests = TestCase $ do
    assertEqual "Rs0"
        (C.pack "")
        $ resync $ C.pack "ABCDEF"
    assertEqual "Rs1"
        (C.pack "\x18\x53\x80\x67Q")
        $ resync $ C.pack "ABCDEF\x18\x53\x80\x67Q"
    assertEqual "Rs2"
        (C.pack "\x18\x53\x80\x67")
        $ resync $ C.pack "\x18\x18\x53\x18\x53\x80\x18\x53\x80\x67"
    assertEqual "Rs3"
        (C.pack "\x18\x53\x80\x67\x18\x53\x80\x67\x18\x53\x80\x67")
        $ resync $ C.pack "\x18\x53\x80\x67\x18\x53\x80\x67\x18\x53\x80\x67"

tests = TestList [
    -- TestLabel "getMajorBit" getMajorBitTests
     TestLabel "readEbmlNumber" readEbmlNumberTests
    ,TestLabel "readBigEndianNumber" readBigEndianNumberTests
    ,TestLabel "readXiphLacingNumber" readXiphLacingNumberTests
    ,TestLabel "tryParseEbml1" tryParseEbml1Tests
    ,TestLabel "resync" resyncTests
    ]

main = runTestTT tests
