{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module MPD.Current.JSON.Types
    ( Tags(..)
    , TagField(..)
    , Status(..)
    , Playlist(..)
    , File(..)
    , State(..)
    , MPDPath(..)
    , MPDPlaybackState(..)
    , MPDId(..)
    ) where

import Network.MPD qualified as MPD

import Data.Aeson.Types
import Deriving.Aeson
import Data.List qualified as L
import Data.Char
import Control.Applicative
import Data.Text qualified as T
import Data.String


data MPDCurrentJSONTag
instance StringModifier MPDCurrentJSONTag where
  getStringModifier s =
    case L.stripPrefix "musicbrainz" s of
      Just xs -> "musicbrainz_" ++ map toLower xs
      Nothing -> camelTo2 '_' s

data TagField = SingleTagField !String
              | MultiTagField ![String]
  deriving stock (Show, Eq, Generic)

{- | Store the parsed output of 'getTag'.

Each field represents a supported MPD tag.
-}
data Tags = Tags
  { artist                    :: !(Maybe TagField)
  , artistSort                :: !(Maybe TagField)
  , album                     :: !(Maybe TagField)
  , albumSort                 :: !(Maybe TagField)
  , albumArtist               :: !(Maybe TagField)
  , albumArtistSort           :: !(Maybe TagField)
  , title                     :: !(Maybe TagField)
  , track                     :: !(Maybe TagField)
  , name                      :: !(Maybe TagField)
  , genre                     :: !(Maybe TagField)
  , date                      :: !(Maybe TagField)
  , originalDate              :: !(Maybe TagField)
  , composer                  :: !(Maybe TagField)
  , performer                 :: !(Maybe TagField)
  , conductor                 :: !(Maybe TagField)
  , work                      :: !(Maybe TagField)
  , grouping                  :: !(Maybe TagField)
  , comment                   :: !(Maybe TagField)
  , disc                      :: !(Maybe TagField)
  , label                     :: !(Maybe TagField)
  , musicbrainzArtistId       :: !(Maybe TagField)
  , musicbrainzAlbumId        :: !(Maybe TagField)
  , musicbrainzAlbumartistId  :: !(Maybe TagField)
  , musicbrainzTrackId        :: !(Maybe TagField)
  , musicbrainzReleasetrackId :: !(Maybe TagField)
  , musicbrainzWorkId         :: !(Maybe TagField)
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via CustomJSON
  '[ FieldLabelModifier '[ MPDCurrentJSONTag ]
   , OmitNothingFields
   ] Tags

instance ToJSON TagField where
  toJSON :: TagField -> Value
  toJSON (SingleTagField s) = toJSON s
  toJSON (MultiTagField maybeList) = toJSON maybeList

instance FromJSON TagField where
  parseJSON v =
        (SingleTagField <$> parseJSON v)
    <|> (MultiTagField <$> parseJSON v)


data Status = Status
  { state          :: !MPDPlaybackState
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
  , updatingDb     :: !(Maybe Int)
  , error          :: !(Maybe String)
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via CustomJSON
  '[ FieldLabelModifier '[ Rename "mixRampDb" "mixramp_db"
                         , Rename "mixRampDelay" "mixramp_delay"
                         , CamelToSnake
                         ]
   , OmitNothingFields
   ] Status

newtype MPDPlaybackState = MPDPlaybackState MPD.PlaybackState
  deriving stock (Show, Eq, Generic)

instance ToJSON MPDPlaybackState where
  toJSON :: MPDPlaybackState -> Value
  toJSON (MPDPlaybackState MPD.Playing) = "playing"
  toJSON (MPDPlaybackState MPD.Paused)  = "paused"
  toJSON (MPDPlaybackState MPD.Stopped) = "stopped"

instance FromJSON MPDPlaybackState where
  parseJSON = withText "MPD.PlaybackState" $ \state -> do
    case state of
      "playing" -> pure (MPDPlaybackState MPD.Playing)
      "paused"  -> pure (MPDPlaybackState MPD.Paused)
      "stopped" -> pure (MPDPlaybackState MPD.Playing)
      _         -> fail $ "Unknown playback state: " ++ show state

data Playlist = Playlist
  { position     :: !(Maybe MPD.Position)
  , nextPosition :: !(Maybe MPD.Position)
  , id           :: !(Maybe MPDId)
  , nextId       :: !(Maybe MPDId)
  , length       :: !Int
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via CustomJSON
  '[ FieldLabelModifier '[ CamelToSnake ]
   , OmitNothingFields
   ] Playlist

newtype MPDId = MPDId MPD.Id
  deriving stock (Show, Eq, Generic)

instance ToJSON MPDId where
  toJSON :: MPDId -> Value
  toJSON (MPDId (MPD.Id i)) = toJSON i

instance FromJSON MPDId where
  parseJSON v = MPDId . MPD.Id <$> parseJSON v

data File = File
  { currentFile :: !(Maybe MPDPath)  -- ^ current song file path
  , nextFile    :: !(Maybe MPDPath)  -- ^ next song file path
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via CustomJSON
  '[ FieldLabelModifier '[ Rename "currentFile" "filename"
                         , Rename "nextFile" "next_filename" ]
   , OmitNothingFields
   ] File

newtype MPDPath = MPDPath MPD.Path
  deriving stock (Show, Eq, Generic)

instance ToJSON MPDPath where
  toJSON :: MPDPath -> Value
  toJSON (MPDPath p) = toJSON $ MPD.toString p

instance FromJSON MPDPath where
  parseJSON = withText "MPD.Path" $ \path -> do
    pure . MPDPath . fromString . T.unpack $ path

-- | Complete MPD State
data State = State
  { mpdFile     :: !File
  , mpdStatus   :: !Status
  , mpdPlaylist :: !Playlist
  , mpdTags     :: !Tags
  , mpdNextTags :: !(Maybe Tags)
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON State where
  toJSON :: State -> Value
  toJSON state = object $
    [ "filename" .= toJSON state.mpdFile.currentFile
    , "status"   .= toJSON state.mpdStatus
    , "playlist" .= toJSON state.mpdPlaylist
    , "tags"     .= state.mpdTags
    ] <> case state.mpdNextTags of
           Nothing -> []
           Just nextTags -> [ "next" .= object
                              [ "filename" .= toJSON state.mpdFile.nextFile
                              , "tags" .= nextTags
                              ] ]
