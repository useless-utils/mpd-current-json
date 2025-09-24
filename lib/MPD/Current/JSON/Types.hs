module MPD.Current.JSON.Types where

import GHC.Generics ( Generic )
import Network.MPD qualified as MPD


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
  { state          :: !(Maybe String)
  , repeat         :: !Bool
  , random         :: !Bool
  , single         :: !Bool
  , consume        :: !Bool
  , duration       :: !(Maybe MPD.FractionalSeconds)  -- Double
  , elapsed        :: !(Maybe MPD.FractionalSeconds)
  , elapsedPercent :: !(Maybe Double)
  , volume         :: !(Maybe Int)
  , audioFormat    :: !(Maybe (Int, Int, Int))
  , bitrate        :: !(Maybe Int)
  , crossfade      :: !(Maybe Int)
  , mixRampDb      :: !(Maybe Double)
  , mixRampDelay   :: !(Maybe Double)
  , updatingDb     :: !(Maybe Bool)
  , error          :: !(Maybe String)
  }
  deriving (Show, Eq, Generic)

data Playlist = Playlist
  { position     :: !(Maybe MPD.Position)
  , nextPosition :: !(Maybe MPD.Position)
  , id           :: !(Maybe MPD.Id)
  , nextId       :: !(Maybe MPD.Id)
  , length       :: !Int
  }
  deriving (Show, Eq, Generic)

data File = File
  { currentFile :: !(Maybe MPD.Path)  -- ^ current song file path
  , nextFile    :: !(Maybe MPD.Path)  -- ^ next song file path
  }
  deriving (Show, Eq, Generic)

-- | Complete MPD State
data State = State
  { mpdFiles    :: !File
  , mpdStatus   :: !MPD.Status
  , mpdPlaylist :: !Playlist
  , mpdTags     :: !Tags
  , mpdNextTags :: !(Maybe Tags)
  }
  deriving (Show, Eq, Generic)
