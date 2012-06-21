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

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B
import Codec.HsMkv.MkvTabular

-- Description of some events

data Info = Info {
     iTimecodeScale :: Integer
    ,iMuxingApplication :: Maybe T.Text
    ,iWritingApplication :: Maybe T.Text
    ,iDuration :: Maybe Double
    ,iDate :: Maybe Double
    ,iSegmentUid :: Maybe T.Text -- hex encoded
    ,iTitle :: Maybe T.Text
} deriving (Show)

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
    TTUnknown Integer
    deriving (Show, Eq, Ord, Read)



data Track = Track {
      tType :: TrackType
    , tNumber :: Integer
    , tCodecId :: T.Text

    , tUID :: Maybe Integer
    , tMinCache :: Maybe Integer
    , tCodecPrivate :: Maybe B.ByteString
    , tDefaultDuration :: Maybe Double
    , tLanguage :: Maybe T.Text
    , tVideoPixelWidth :: Maybe Integer
    , tVideoPixelHeight :: Maybe Integer
    , tVideoDisplayWidth :: Maybe Integer
    , tVideoDisplayHeight :: Maybe Integer
    , tAudioSamplingFrequency :: Maybe Double
    , tAudioOutputSamplingFrequency :: Maybe Double
    , tAudioChannels :: Maybe Integer
    } deriving (Show)

blankTrack :: Track
blankTrack = Track (TTUnknown (-1)) (-1) T.empty Nothing Nothing Nothing Nothing 
    Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

data Frame = Frame {
     fTrackNumber :: Integer
    ,fTimeCode :: Double
    ,fData :: [B.ByteString]
    ,fDuration :: Maybe Double
    ,fInvisible :: Bool
    ,fDiscardable :: Bool
    ,fKeyframe :: Bool
} deriving (Show)

blankFrame :: Frame
blankFrame = Frame (-1) (-1.0) [] Nothing False False False

data MatroskaEvent =
    MEFrame Frame |
    METracks [Track] |
    MEInfo Info |

    MEEbmlElement MatroskaElement |
    MEResync |
    MENoop
    deriving (Show)



data MatroskaElement = MatroskaElement {
     meClass :: ElementClass
    ,meSize :: Maybe Integer
    ,meContent :: ElementContent  
    } deriving (Show, Eq)

data ElementContent = 
        ECMaster [MatroskaElement] |
        ECUnsigned Integer |
        ECSigned Integer |
        ECTextAscii T.Text |
        ECTextUtf8 T.Text |
        ECBinary B.ByteString |
        ECFloat Double |
        ECDate Double
        deriving (Show, Eq)


-- State of the parsing. 
-- There can be nested ParserStates when parsing child elements.

data MatroskaLacingType = NoLacing | XiphLacing | FixedSizeLacing | EbmlLacing deriving (Show)

data EbmlNumberType = ENUnsigned | ENSigned | ENUnmodified  deriving (Show, Eq, Ord)
