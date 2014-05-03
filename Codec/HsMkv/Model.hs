module Codec.HsMkv.Model (
     Info(..)
    ,TrackType(..)
    ,Track(..)
    ,Frame(..)
    ,MatroskaEvent(..)
    ,MatroskaElement(..)
    ,ElementContent(..)
    ,MatroskaLacingType(..)
    ,blankInfo
    ,blankTrack
    ,blankFrame
    ,EbmlNumberType(..)
    ) where

import Data.Int
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B
import Codec.HsMkv.MkvTabular

-- Description of some events

data Info = Info {
     iTimecodeScale         :: Int64
    ,iMuxingApplication     :: Maybe T.Text
    ,iWritingApplication    :: Maybe T.Text
    ,iDuration              :: Maybe Double
    ,iDate                  :: Maybe Double
    ,iSegmentUid            :: Maybe T.Text -- hex encoded
    ,iTitle                 :: Maybe T.Text
} deriving (Show, Eq, Read)

blankInfo :: Info
blankInfo = Info 1000000 Nothing Nothing Nothing Nothing Nothing Nothing


data TrackType = 
    TTAudio | 
    TTVideo | 
    TTSubtitle | 
    TTComplex | 
    TTLogo | 
    TTButton | 
    TTControl | 
    TTUnknown Int64
    deriving (Show, Eq, Ord, Read)



data Track = Track {
      tType         :: TrackType
    , tNumber       :: Int64
    , tCodecId      :: T.Text

    , tUID          :: Maybe Int64
    , tMinCache     :: Maybe Int64
    , tCodecPrivate :: Maybe B.ByteString
    , tDefaultDuration :: Maybe Double
    , tLanguage     :: Maybe T.Text
    , tVideoPixelWidth :: Maybe Int64
    , tVideoPixelHeight :: Maybe Int64
    , tVideoDisplayWidth :: Maybe Int64
    , tVideoDisplayHeight :: Maybe Int64
    , tVideoColourSpace :: Maybe B.ByteString
    , tAudioSamplingFrequency :: Maybe Double
    , tAudioOutputSamplingFrequency :: Maybe Double
    , tAudioChannels :: Maybe Int64
    , tHeaderCompressionPrefix :: Maybe B.ByteString
    } deriving (Show, Eq, Read)

blankTrack :: Track
blankTrack = Track (TTUnknown (-1)) (-1) T.empty Nothing Nothing Nothing Nothing 
    Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

data Frame = Frame {
     fTrackNumber :: Int64
    ,fTimeCode :: Double
    ,fData :: [B.ByteString]
    ,fDuration :: Maybe Double
    ,fInvisible :: Bool
    ,fDiscardable :: Bool
    ,fKeyframe :: Bool
} deriving (Show, Eq, Read)

blankFrame :: Frame
blankFrame = Frame (-1) (-1.0) [] Nothing False False False

data MatroskaEvent =
    MEFrame Frame |
    METracks [Track] |
    MEInfo Info |

    MEEbmlElement MatroskaElement |
    MEResync |
    MENoop
    deriving (Show, Read, Eq)



data MatroskaElement = MatroskaElement {
     meClass :: ElementClass
    ,meSize :: Maybe Int64
    ,meContent :: ElementContent  
    } deriving (Show, Eq, Read)

data ElementContent = 
        ECMaster [MatroskaElement] |
        ECUnsigned Int64 |
        ECSigned Int64 |
        ECTextAscii T.Text |
        ECTextUtf8 T.Text |
        ECBinary B.ByteString |
        ECFloat Double |
        ECDate Double
        deriving (Show, Eq, Read)


-- State of the parsing. 
-- There can be nested ParserStates when parsing child elements.

data MatroskaLacingType = NoLacing | XiphLacing | FixedSizeLacing | EbmlLacing deriving (Show, Eq, Ord, Read)

data EbmlNumberType = ENUnsigned | ENSigned | ENUnmodified  deriving (Show, Eq, Ord, Read)
