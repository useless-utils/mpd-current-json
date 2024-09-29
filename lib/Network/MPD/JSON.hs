{-# LANGUAGE OverloadedStrings #-}
module Network.MPD.JSON
  ( objectMaybes
  , jsonSongTags
  , tagFieldToJSON
  , (.=?)
  )
where

import Network.MPD.Parse
    ( ExtractedTags(..), TagField(..) )
import Data.Aeson
    ( Value, KeyValue((.=)), ToJSON(toJSON), Key, object )
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Aeson.Types ( Pair )
import Data.Maybe ( catMaybes )

{- | Helper function for creating an JSON 'Data.Aeson.object' where
'Data.Maybe.catMaybes' won't include items from the @[Maybe
'Data.Aeson.Types.Pair']@ list that return 'Nothing'.

Meant for using with the '(.=?)' operator to remove JSON values from
the output that would contain @null@ otherwise.
-}
objectMaybes :: [Maybe Pair] -> Value
objectMaybes = object . catMaybes

-- | Create a 'Data.Aeson.Value' that can be encoded into a
-- @ByteString@ of conventional JSON with 'Data.Aeson.encode'.
jsonSongTags :: ExtractedTags -> Value
jsonSongTags song = objectMaybes
  [ "artist"            .=? tagFieldToJSON (artist          song)
  , "artist_sort"       .=? tagFieldToJSON (artistSort      song)
  , "album"             .=? tagFieldToJSON (album           song)
  , "album_sort"        .=? tagFieldToJSON (albumSort       song)
  , "album_artist"      .=? tagFieldToJSON (albumArtist     song)
  , "album_artist_sort" .=? tagFieldToJSON (albumArtistSort song)
  , "title"             .=? tagFieldToJSON (title           song)
  , "track"             .=? tagFieldToJSON (track           song)
  , "name"              .=? tagFieldToJSON (name            song)
  , "genre"             .=? tagFieldToJSON (genre           song)
  , "date"              .=? tagFieldToJSON (date            song)
  , "original_date"     .=? tagFieldToJSON (originalDate    song)
  , "composer"          .=? tagFieldToJSON (composer        song)
  , "performer"         .=? tagFieldToJSON (performer       song)
  , "conductor"         .=? tagFieldToJSON (conductor       song)
  , "work"              .=? tagFieldToJSON (work            song)
  , "grouping"          .=? tagFieldToJSON (grouping        song)
  , "comment"           .=? tagFieldToJSON (comment         song)
  , "disc"              .=? tagFieldToJSON (disc            song)
  , "label"             .=? tagFieldToJSON (label           song)
  , "musicbrainz_artistid"       .=? tagFieldToJSON (musicbrainz_ArtistId       song)
  , "musicbrainz_albumid"        .=? tagFieldToJSON (musicbrainz_AlbumId        song)
  , "musicbrainz_albumartistid"  .=? tagFieldToJSON (musicbrainz_AlbumartistId  song)
  , "musicbrainz_trackid"        .=? tagFieldToJSON (musicbrainz_TrackId        song)
  , "musicbrainz_releasetrackid" .=? tagFieldToJSON (musicbrainz_ReleasetrackId song)
  , "musicbrainz_workid"         .=? tagFieldToJSON (musicbrainz_WorkId         song)
  ]

-- | Convert constructor arguments of 'TagField', specially @String@
-- or @[String]@ under @Maybe@, into a 'Data.Aeson.Value' supported
-- for encoding. Since 'jsonSongTags' expects @Maybe Value@, extract
-- them from 'TagField'.
tagFieldToJSON :: TagField -> Maybe Value
tagFieldToJSON (SingleTagField ms) = toJSON <$> ms
tagFieldToJSON (MultiTagField ml) = toJSON <$> ml

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
