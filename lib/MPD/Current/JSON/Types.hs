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
  { state          :: !MPD.PlaybackState
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

instance ToJSON MPD.PlaybackState where
  toJSON :: MPD.PlaybackState -> Value
  toJSON MPD.Playing = "playing"
  toJSON MPD.Paused  = "paused"
  toJSON MPD.Stopped = "stopped"

instance FromJSON MPD.PlaybackState where
  parseJSON = withText "MPD.PlaybackState" $ \state -> do
    case state of
      "playing" -> pure MPD.Playing
      "paused"  -> pure MPD.Paused
      "stopped" -> pure MPD.Playing
      _         -> fail $ "Unknown playback state: " ++ show state

data Playlist = Playlist
  { position     :: !(Maybe MPD.Position)
  , nextPosition :: !(Maybe MPD.Position)
  , id           :: !(Maybe MPD.Id)
  , nextId       :: !(Maybe MPD.Id)
  , length       :: !Int
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via CustomJSON
  '[ FieldLabelModifier '[ CamelToSnake ]
   , OmitNothingFields
   ] Playlist

instance ToJSON MPD.Id where
  toJSON :: MPD.Id -> Value
  toJSON (MPD.Id i) = toJSON $ i

instance FromJSON MPD.Id where
  parseJSON v = MPD.Id <$> parseJSON v

data File = File
  { currentFile :: !(Maybe MPD.Path)  -- ^ current song file path
  , nextFile    :: !(Maybe MPD.Path)  -- ^ next song file path
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via CustomJSON
  '[ FieldLabelModifier '[ Rename "currentFile" "filename"
                         , Rename "nextFile" "next_filename" ]
   , OmitNothingFields
   ] File

instance ToJSON MPD.Path where
  toJSON p = toJSON $ MPD.toString p

instance FromJSON MPD.Path where
  parseJSON = withText "MPD.Path" $ \path -> do
    pure $ fromString . T.unpack $ path

-- newtype MPDPathToJSON = MPDPathToJSON MPD.Path
-- instance ToJSON MPDPathToJSON where
--   toJSON :: MPDPathToJSON -> Value
--   toJSON (MPDPathToJSON p) = toJSON $ MPD.toString p

-- | Complete MPD State
data State = State
  { mpdFiles    :: !File
  , mpdStatus   :: !Status
  , mpdPlaylist :: !Playlist
  , mpdTags     :: !Tags
  , mpdNextTags :: !(Maybe Tags)
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON State where
  toJSON :: State -> Value
  toJSON state = object $
    [ "filename" .= toJSON state.mpdFiles.currentFile
    , "status"   .= toJSON state.mpdStatus
    , "playlist" .= toJSON state.mpdPlaylist
    , "tags"     .= state.mpdTags
    ] <> case state.mpdNextTags of
           Nothing -> []
           Just nextTags -> [ "next" .= object
                              [ "filename" .= toJSON state.mpdFiles.nextFile
                              , "tags" .= nextTags
                              ] ]
