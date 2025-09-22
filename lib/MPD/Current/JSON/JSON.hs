{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module MPD.Current.JSON.JSON where

import           MPD.Current.JSON.Types
import qualified Network.MPD as MPD

import qualified Data.Aeson.KeyMap as KM
import           Data.Aeson.Types
import           Data.Maybe

{- | Helper function for creating an JSON 'Data.Aeson.object' where
'Data.Maybe.catMaybes' won't include items from the @[Maybe
'Data.Aeson.Types.Pair']@ list that return 'Nothing'.

Meant for using with the '(.=?)' operator to remove JSON values from
the output that would contain @null@ otherwise.
-}
objectMaybes :: [Maybe Pair] -> Value
objectMaybes = object . catMaybes

-- | Convert constructor arguments of 'TagField', specially @String@
-- or @[String]@ under @Maybe@, into a 'Data.Aeson.Value' supported
-- for encoding. Since 'jsonSongTags' expects @Maybe Value@, extract
-- them from 'TagField'.
-- tagFieldToJSON

{- | Check if @Maybe v@ exists and is of type expected by
'Data.Aeson.object' as defined in 'Data.Aeson.Value', if it is return
both the @key@ and @value@ within the @Maybe@ context tied with
'Data.Aeson..='. This gives support to \'optional\' fields using
'Data.Maybe.catMaybes' that discard @Nothing@ values and is meant to
prevent creating JSON key/value pairs with @null@ values, e.g.:

@
jsonTags = object . catMaybes $
    [ "artist"  .=? artist
    , "album"   .=? album
    , "title"   .=? title
    ]
@

Where if a value on the right is @Nothing@ that key/value pair will
not be included in 'Data.Aeson.object' because of
'Data.Maybe.catMaybes'.
-}
(.=?) :: (KeyValue e a, ToJSON v) => Key -> Maybe v -> Maybe a
key .=? Just value = Just (key .= value)
_   .=? Nothing    = Nothing
infixr 8 .=?


-- | Type class for optional JSON serialization
class OptionalToJSON a where
  tagFieldToJSON :: a -> Maybe Value

instance OptionalToJSON TagField where
  tagFieldToJSON (SingleTagField ms) = toJSON <$> ms
  tagFieldToJSON (MultiTagField ml) = toJSON <$> ml

-- | Enhanced operator that works like .= but omits Nothing values
(.=??) :: OptionalToJSON a => Key -> a -> Maybe Pair
key .=?? field = (key .=) <$> tagFieldToJSON field
infixr 8 .=??


instance ToJSON Tags where
  toJSON tag
    = objectMaybes
      [ "artist"                     .=?? tag.artist
      , "artist_sort"                .=?? tag.artistSort
      , "album"                      .=?? tag.album
      , "album_sort"                 .=?? tag.albumSort
      , "album_artist"               .=?? tag.albumArtist
      , "album_artist_sort"          .=?? tag.albumArtistSort
      , "title"                      .=?? tag.title
      , "track"                      .=?? tag.track
      , "name"                       .=?? tag.name
      , "genre"                      .=?? tag.genre
      , "date"                       .=?? tag.date
      , "original_date"              .=?? tag.originalDate
      , "composer"                   .=?? tag.composer
      , "performer"                  .=?? tag.performer
      , "conductor"                  .=?? tag.conductor
      , "work"                       .=?? tag.work
      , "grouping"                   .=?? tag.grouping
      , "comment"                    .=?? tag.comment
      , "disc"                       .=?? tag.disc
      , "label"                      .=?? tag.label
      , "musicbrainz_artistid"       .=?? tag.musicbrainzArtistId
      , "musicbrainz_albumid"        .=?? tag.musicbrainzAlbumId
      , "musicbrainz_albumartistid"  .=?? tag.musicbrainzAlbumartistId
      , "musicbrainz_trackid"        .=?? tag.musicbrainzTrackId
      , "musicbrainz_releasetrackid" .=?? tag.musicbrainzReleasetrackId
      , "musicbrainz_workid"         .=?? tag.musicbrainzWorkId
      ]

instance ToJSON Status where
  toJSON ps = objectMaybes
    [ "state"           .=? Just ps.psState
    , "repeat"          .=? Just ps.psRepeat
    , "random"          .=? Just ps.psRandom
    , "single"          .=? Just ps.psSingle
    , "consume"         .=? Just ps.psConsume
    , "duration"        .=? ps.psDuration
    , "elapsed"         .=? ps.psElapsed
    , "elapsed_percent" .=? ps.psElapsedPercent
    , "volume"          .=? ps.psVolume
    , "audio_format"    .=? ps.psAudioFormat
    , "bitrate"         .=? ps.psBitrate
    , "crossfade"       .=? ps.psCrossfade
    , "mixramp_db"      .=? ps.psMixRampDb
    , "mixramp_delay"   .=? ps.psMixRampDelay
    , "updating_db"     .=? ps.psUpdatingDb
    , "error"           .=? ps.psError
    ]

instance ToJSON PlaylistInfo where
  toJSON pi = objectMaybes
    [ "position"      .=? pi.piPosition
    , "next_position" .=? pi.piNextPosition
    , "id"            .=? pi.piId
    , "next_id"       .=? pi.piNextId
    , "length"        .=? pi.piLength
    ]

instance ToJSON FileInfo where
  toJSON fi = objectMaybes
    [ "filename" .=? fi.fiCurrentFile
    , "next_filename" .=? fi.fiNextFile
    ]

instance ToJSON MPDState where
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
