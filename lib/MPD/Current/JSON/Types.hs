{-# LANGUAGE DeriveGeneric #-}

module MPD.Current.JSON.Types where


import           GHC.Generics
import qualified Network.MPD as MPD


data TagField = SingleTagField !(Maybe String)
              | MultiTagField !(Maybe [String])
  deriving (Show, Eq)

{- | Store the parsed output of 'getTag'.

Each field represents a supported MPD tag.
-}
data Tags = Tags
  { artist                    :: !TagField
  , artistSort                :: !TagField
  , album                     :: !TagField
  , albumSort                 :: !TagField
  , albumArtist               :: !TagField
  , albumArtistSort           :: !TagField
  , title                     :: !TagField
  , track                     :: !TagField
  , name                      :: !TagField
  , genre                     :: !TagField
  , date                      :: !TagField
  , originalDate              :: !TagField
  , composer                  :: !TagField
  , performer                 :: !TagField
  , conductor                 :: !TagField
  , work                      :: !TagField
  , grouping                  :: !TagField
  , comment                   :: !TagField
  , disc                      :: !TagField
  , label                     :: !TagField
  , musicbrainzArtistId       :: !TagField
  , musicbrainzAlbumId        :: !TagField
  , musicbrainzAlbumartistId  :: !TagField
  , musicbrainzTrackId        :: !TagField
  , musicbrainzReleasetrackId :: !TagField
  , musicbrainzWorkId         :: !TagField
  }
  deriving (Show, Eq, Generic)

data Status = Status
  { psState          :: !(Maybe String)
  , psRepeat         :: !Bool
  , psRandom         :: !Bool
  , psSingle         :: !Bool
  , psConsume        :: !Bool
  , psDuration       :: !(Maybe MPD.FractionalSeconds)  -- Double
  , psElapsed        :: !(Maybe MPD.FractionalSeconds)
  , psElapsedPercent :: !(Maybe Double)
  , psVolume         :: !(Maybe Int)
  , psAudioFormat    :: !(Maybe (Int, Int, Int))
  , psBitrate        :: !(Maybe Int)
  , psCrossfade      :: !(Maybe Int)
  , psMixRampDb      :: !(Maybe Double)
  , psMixRampDelay   :: !(Maybe Double)
  , psUpdatingDb     :: !(Maybe Bool)
  , psError          :: !(Maybe String)
  }
  deriving (Show, Eq, Generic)

data PlaylistInfo = PlaylistInfo
  { piPosition     :: !(Maybe MPD.Position)
  , piNextPosition :: !(Maybe MPD.Position)
  , piId           :: !(Maybe Int)
  , piNextId       :: !(Maybe Int)
  , piLength       :: !(Maybe Int)
  }
  deriving (Show, Eq, Generic)

-- | File Information
data FileInfo = FileInfo
  { fiCurrentFile :: !(Maybe String)  -- ^ current song file path
  , fiNextFile    :: !(Maybe String)  -- ^ next song file path
  }
  deriving (Show, Eq, Generic)

-- | Complete MPD State
data MPDState = MPDState
  { mpdFiles    :: !FileInfo
  , mpdStatus   :: !Status
  , mpdPlaylist :: !PlaylistInfo
  , mpdTags     :: !Tags
  , mpdNextTags :: !(Maybe Tags)
  }
  deriving (Show, Eq, Generic)
