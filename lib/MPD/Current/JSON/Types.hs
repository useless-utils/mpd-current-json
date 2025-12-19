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

import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types
import Data.Maybe ( catMaybes )

import Deriving.Aeson
import Data.List qualified as L
import Data.Char
import Control.Applicative

data MPDCurrentJSONTag

instance StringModifier MPDCurrentJSONTag where
  getStringModifier s =
    case L.stripPrefix "musicbrainz" s of
      Just xs -> "musicbrainz_" ++ map toLower xs
      Nothing -> camelTo2 '_' s

data MPDCurrentJSONStatus

instance StringModifier MPDCurrentJSONStatus where
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

data Playlist = Playlist
  { position     :: !(Maybe MPD.Position)
  , nextPosition :: !(Maybe MPD.Position)
  , id           :: !(Maybe MPD.Id)
  , nextId       :: !(Maybe MPD.Id)
  , length       :: !Int
  }
  deriving stock (Show, Eq, Generic)

data File = File
  { currentFile :: !(Maybe MPD.Path)  -- ^ current song file path
  , nextFile    :: !(Maybe MPD.Path)  -- ^ next song file path
  }
  deriving stock (Show, Eq, Generic)

-- | Complete MPD State
data State = State
  { mpdFiles    :: !File
  , mpdStatus   :: !Status
  , mpdPlaylist :: !Playlist
  , mpdTags     :: !Tags
  , mpdNextTags :: !(Maybe Tags)
  }
  deriving stock (Show, Eq, Generic)

-- | JSON Instances

{- | Helper function for creating an JSON 'Data.Aeson.object' where
'Data.Maybe.catMaybes' won't include items from the @[Maybe
'Data.Aeson.Types.Pair']@ list that return 'Nothing'.

Meant for using with the '(.=?)' operator to remove JSON values from
the output that would contain @null@ otherwise.
-}
objectMaybes :: [Maybe Pair] -> Value
objectMaybes = object . catMaybes

(.=?) :: (MaybeToJSON a) => Key -> a -> Maybe Pair
key .=? value = (key .=) <$> maybeToJSON value
infixr 8 .=?

-- | Type class for optional JSON serialization
class MaybeToJSON a where
  maybeToJSON :: a -> Maybe Value

instance (ToJSON a) => MaybeToJSON (Maybe a) where
  maybeToJSON :: Maybe a -> Maybe Value
  maybeToJSON (Just a) = Just (toJSON a)
  maybeToJSON Nothing = Nothing

instance MaybeToJSON Bool where
  maybeToJSON :: Bool -> Maybe Value
  maybeToJSON = Just . toJSON

-- Numbers: Always included
instance MaybeToJSON Int where
  maybeToJSON :: Int -> Maybe Value
  maybeToJSON = Just . toJSON

instance MaybeToJSON Double where
  maybeToJSON :: Double -> Maybe Value
  maybeToJSON = Just . toJSON

-- Lists: Empty lists omitted
instance (ToJSON a) => MaybeToJSON [a] where
  maybeToJSON :: [a] -> Maybe Value
  maybeToJSON [] = Nothing
  maybeToJSON xs = Just (toJSON xs)

instance MaybeToJSON MPD.PlaybackState where
  maybeToJSON :: MPD.PlaybackState -> Maybe Value
  maybeToJSON MPD.Playing = Just "playing"
  maybeToJSON MPD.Paused  = Just "pause"
  maybeToJSON MPD.Stopped = Just "stopped"

instance MaybeToJSON (Int, Int, Int) where
  maybeToJSON :: (Int, Int, Int) -> Maybe Value
  maybeToJSON = Just . toJSON

instance MaybeToJSON MPD.Seconds where
  maybeToJSON :: MPD.Seconds -> Maybe Value
  maybeToJSON = Just . toJSON

instance MaybeToJSON MPD.Id where
  maybeToJSON :: MPD.Id -> Maybe Value
  maybeToJSON (MPD.Id i) = Just . toJSON $ i

-- Orphans
newtype MPDIdToJSON = MPDIdToJSON MPD.Id
newtype MPDPathToJSON = MPDPathToJSON MPD.Path

instance ToJSON MPDIdToJSON where
  toJSON :: MPDIdToJSON -> Value
  toJSON (MPDIdToJSON (MPD.Id i)) = toJSON i

instance ToJSON MPDPathToJSON where
  toJSON :: MPDPathToJSON -> Value
  toJSON (MPDPathToJSON p) = toJSON $ MPD.toString p

instance ToJSON Status where
  toJSON :: Status -> Value
  toJSON st = objectMaybes
    [ "state"           .=? st.state
    , "repeat"          .=? st.repeat
    , "random"          .=? st.random
    , "single"          .=? st.single
    , "consume"         .=? st.consume
    , "duration"        .=? st.duration
    , "elapsed"         .=? st.elapsed
    , "elapsed_percent" .=? st.elapsedPercent
    , "volume"          .=? st.volume
    , "audio_format"    .=? st.audioFormat
    , "bitrate"         .=? st.bitrate
    , "crossfade"       .=? st.crossfade
    , "mixramp_db"      .=? st.mixRampDb
    , "mixramp_delay"   .=? st.mixRampDelay
    , "updating_db"     .=? st.updatingDb
    , "error"           .=? st.error
    ]

instance ToJSON Playlist where
  toJSON :: Playlist -> Value
  toJSON pl = objectMaybes
    [ "position"      .=? pl.position
    , "next_position" .=? pl.nextPosition
    , "id"            .=? (MPDIdToJSON <$> pl.id)
    , "next_id"       .=? (MPDIdToJSON <$> pl.nextId)
    , "length"        .=? pl.length
    ]

instance ToJSON File where
  toJSON :: File -> Value
  toJSON fi = objectMaybes
    [ "filename"      .=? (MPDPathToJSON <$> fi.currentFile)
    , "next_filename" .=? (MPDPathToJSON <$> fi.nextFile)
    ]

instance ToJSON State where
  toJSON :: State -> Value
  toJSON state = object $ concat
    [ objectPairs (toJSON state.mpdFiles)
    , [ "status"   .= toJSON state.mpdStatus
      , "playlist" .= toJSON state.mpdPlaylist
      , "tags"     .= state.mpdTags
      ]
    , case state.mpdNextTags of
        Nothing -> []
        Just nextTags -> ["next" .= object ["tags" .= nextTags]]
    ]
    where
      objectPairs (Object obj) = [(k, v) | (k, v) <- KM.toList obj]
      objectPairs _ = []


