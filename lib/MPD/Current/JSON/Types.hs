{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module MPD.Current.JSON.Types
    ( Tags(..)
    , TagField(..)
    , Status(..)
    , Playlist(..)
    , File(..)
    , State(..)
    ) where

import GHC.Generics ( Generic )
import Network.MPD qualified as MPD

import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types
    ( Key,
      object,
      Pair,
      Value(Object),
      KeyValue((.=)),
      ToJSON(toJSON) )
import Data.Maybe ( catMaybes )


data TagField = SingleTagField !(Maybe String)
              | MultiTagField !(Maybe [String])
  deriving stock (Show, Eq)

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
  deriving stock (Show, Eq, Generic)

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

instance MaybeToJSON TagField where
  maybeToJSON :: TagField -> Maybe Value
  maybeToJSON (SingleTagField ms) = toJSON <$> ms
  maybeToJSON (MultiTagField ml) = toJSON <$> ml

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


instance ToJSON Tags where
  toJSON :: Tags -> Value
  toJSON tag = objectMaybes
    [ "artist"                     .=? tag.artist
    , "artist_sort"                .=? tag.artistSort
    , "album"                      .=? tag.album
    , "album_sort"                 .=? tag.albumSort
    , "album_artist"               .=? tag.albumArtist
    , "album_artist_sort"          .=? tag.albumArtistSort
    , "title"                      .=? tag.title
    , "track"                      .=? tag.track
    , "name"                       .=? tag.name
    , "genre"                      .=? tag.genre
    , "date"                       .=? tag.date
    , "original_date"              .=? tag.originalDate
    , "composer"                   .=? tag.composer
    , "performer"                  .=? tag.performer
    , "conductor"                  .=? tag.conductor
    , "work"                       .=? tag.work
    , "grouping"                   .=? tag.grouping
    , "comment"                    .=? tag.comment
    , "disc"                       .=? tag.disc
    , "label"                      .=? tag.label
    , "musicbrainz_artistid"       .=? tag.musicbrainzArtistId
    , "musicbrainz_albumid"        .=? tag.musicbrainzAlbumId
    , "musicbrainz_albumartistid"  .=? tag.musicbrainzAlbumartistId
    , "musicbrainz_trackid"        .=? tag.musicbrainzTrackId
    , "musicbrainz_releasetrackid" .=? tag.musicbrainzReleasetrackId
    , "musicbrainz_workid"         .=? tag.musicbrainzWorkId
    ]

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


