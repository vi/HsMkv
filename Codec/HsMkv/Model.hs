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
     i_timecodeScale :: Integer
    ,i_muxingApplication :: Maybe T.Text
    ,i_writingApplication :: Maybe T.Text
    ,i_duration :: Maybe Double
    ,i_date :: Maybe Double
    ,i_segmentUid :: Maybe T.Text -- hex encoded
    ,i_title :: Maybe T.Text
} deriving (Show)

blankInfo :: Info
blankInfo = Info 1000000 Nothing Nothing Nothing Nothing Nothing Nothing


data TrackType = 
    TT_Audio | 
    TT_Video | 
    TT_Subtitle | 
    TT_Complex | 
    TT_Logo | 
    TT_Button | 
    TT_Control | 
    TT_Unknown Integer
    deriving (Show, Eq, Ord, Read)



data Track = Track {
      t_type :: TrackType
    , t_number :: Integer
    , t_codecId :: T.Text

    , t_UID :: Maybe Integer
    , t_minCache :: Maybe Integer
    , t_codecPrivate :: Maybe B.ByteString
    , t_defaultDuration :: Maybe Double
    , t_language :: Maybe T.Text
    , t_videoPixelWidth :: Maybe Integer
    , t_videoPixelHeight :: Maybe Integer
    , t_videoDisplayWidth :: Maybe Integer
    , t_videoDisplayHeight :: Maybe Integer
    , t_audioSamplingFrequency :: Maybe Double
    , t_audioOutputSamplingFrequency :: Maybe Double
    , t_audioChannels :: Maybe Integer
    } deriving (Show)

blankTrack :: Track
blankTrack = Track (TT_Unknown (-1)) (-1) T.empty Nothing Nothing Nothing Nothing 
    Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

data Frame = Frame {
     f_trackNumber :: Integer
    ,f_timeCode :: Double
    ,f_data :: [B.ByteString]
    ,f_duration :: Maybe Double
    ,f_invisible :: Bool
    ,f_discardable :: Bool
    ,f_keyframe :: Bool
} deriving (Show)

blankFrame :: Frame
blankFrame = Frame (-1) (-1.0) [] Nothing False False False

data MatroskaEvent =
    ME_Frame Frame |
    ME_Tracks [Track] |
    ME_Info Info |

    ME_EbmlElement MatroskaElement |
    ME_Resync |
    ME_Noop
    deriving (Show)



data MatroskaElement = MatroskaElement {
     me_class :: ElementClass
    ,me_size :: Maybe Integer
    ,me_content :: ElementContent  
    } deriving (Show, Eq)

data ElementContent = 
        EC_Master [MatroskaElement] |
        EC_Unsigned Integer |
        EC_Signed Integer |
        EC_TextAscii T.Text |
        EC_TextUtf8 T.Text |
        EC_Binary B.ByteString |
        EC_Float Double |
        EC_Date Double
        deriving (Show, Eq)


-- State of the parsing. 
-- There can be nested ParserStates when parsing child elements.

data MatroskaLacingType = NoLacing | XiphLacing | FixedSizeLacing | EbmlLacing deriving (Show)

data EbmlNumberType = ENUnsigned | ENSigned | ENUnmodified  deriving (Show, Eq, Ord)
