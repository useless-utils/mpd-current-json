{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}


module MPD.Current.JSON.JSON where

import MPD.Current.JSON.Types (TagField(..))
import MPD.Current.JSON.Types qualified as Current
import MPD.Current.JSON.Parse ()
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
import Text.Read ( readMaybe )
import Text.Printf ( printf )


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
  maybeToJSON (SingleTagField ms) = toJSON <$> ms
  maybeToJSON (MultiTagField ml) = toJSON <$> ml

instance (ToJSON a) => MaybeToJSON (Maybe a) where
  maybeToJSON (Just a) = Just (toJSON a)
  maybeToJSON Nothing = Nothing

instance MaybeToJSON Bool where
  maybeToJSON = Just . toJSON

-- Numbers: Always included
instance MaybeToJSON Int where
  maybeToJSON = Just . toJSON

instance MaybeToJSON Double where
  maybeToJSON = Just . toJSON

-- Lists: Empty lists omitted
instance (ToJSON a) => MaybeToJSON [a] where
  maybeToJSON [] = Nothing
  maybeToJSON xs = Just (toJSON xs)

instance MaybeToJSON MPD.PlaybackState where
  maybeToJSON MPD.Playing = Just "playing"
  maybeToJSON MPD.Paused  = Just "pause"
  maybeToJSON MPD.Stopped = Just "stopped"

instance MaybeToJSON (Int, Int, Int) where
  maybeToJSON = Just . toJSON

instance MaybeToJSON MPD.Seconds where
  maybeToJSON = Just . toJSON

instance MaybeToJSON MPD.Id where
  maybeToJSON (MPD.Id i) = Just . toJSON $ i

instance ToJSON MPD.Id where
  toJSON (MPD.Id i) = toJSON i

instance ToJSON MPD.Path where
  toJSON p = toJSON $ MPD.toString p


instance ToJSON Current.Tags where
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

instance ToJSON Current.Status where
  toJSON ps = objectMaybes
    [ "state"           .=? ps.state
    , "repeat"          .=? ps.repeat
    , "random"          .=? ps.random
    , "single"          .=? ps.single
    , "consume"         .=? ps.consume
    , "duration"        .=? ps.duration
    , "elapsed"         .=? ps.elapsed
    , "elapsed_percent" .=? ps.elapsedPercent
    , "volume"          .=? ps.volume
    , "audio_format"    .=? ps.audioFormat
    , "bitrate"         .=? ps.bitrate
    , "crossfade"       .=? ps.crossfade
    , "mixramp_db"      .=? ps.mixRampDb
    , "mixramp_delay"   .=? ps.mixRampDelay
    , "updating_db"     .=? ps.updatingDb
    , "error"           .=? ps.error
    ]

instance ToJSON MPD.Status where
  toJSON st = objectMaybes
    [ "state"           .=? st.stState
    , "repeat"          .=? st.stRepeat
    , "random"          .=? st.stRandom
    , "single"          .=? st.stSingle
    , "consume"         .=? st.stConsume
    , "duration"        .=? fmap snd st.stTime
    , "elapsed"         .=? fmap fst st.stTime
    , "elapsed_percent" .=? calcElapsedPercent st.stTime
    , "volume"          .=? fmap toInteger st.stVolume
    , "audio_format"    .=? st.stAudio
    , "bitrate"         .=? st.stBitrate
    , "crossfade"       .=? st.stXFadeWidth
    , "mixramp_db"      .=? st.stMixRampdB
    , "mixramp_delay"   .=? st.stMixRampDelay
    , "updating_db"     .=? st.stUpdatingDb
    , "error"           .=? st.stError
    ]
    where
      calcElapsedPercent :: Maybe (MPD.FractionalSeconds, MPD.FractionalSeconds) -> Maybe Double
      calcElapsedPercent Nothing = Nothing
      calcElapsedPercent (Just (elapsed, duration)) = do
        let elapsedPercent = (elapsed / duration) * 100
        if duration > 0
          then readMaybe $ printf "%02.2f" elapsedPercent :: Maybe Double
          else Nothing

instance ToJSON Current.Playlist where
  toJSON pi = objectMaybes
    [ "position"      .=? pi.position
    , "next_position" .=? pi.nextPosition
    , "id"            .=? pi.id
    , "next_id"       .=? pi.nextId
    , "length"        .=? pi.length
    ]

instance ToJSON Current.File where
  toJSON fi = objectMaybes
    [ "filename"      .=? fi.currentFile
    , "next_filename" .=? fi.nextFile
    ]

instance ToJSON Current.State where
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

