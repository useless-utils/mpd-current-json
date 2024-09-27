{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import qualified Network.MPD as MPD
import Network.MPD
       ( PlaybackState(Stopped, Playing, Paused) )
import Data.Aeson ( object, KeyValue((.=)) )
import Data.Aeson.Encode.Pretty
       ( defConfig, encodePretty', keyOrder, Config(confCompare) )
import qualified Data.ByteString.Lazy.Char8 as C
import Text.Printf ( printf )
import Options
       ( optsParserInfo, execParser, Opts(..), NextSongFlag(..) )

import Network.MPD.Parse
       -- ( getStatusField
       -- , getStatusFieldElement
       -- , getAllTags
       -- , maybePathCurrentSong
       -- , maybePathNextPlaylistSong
       -- , (.=?)
       -- , objectJson
       -- , getStatusIdInt
       -- , tagFieldToMaybeString )

import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
{- | Where the program connects to MPD and uses the helper functions to
extract values, organize them into a list of key/value pairs, make
them a 'Data.Aeson.Value' using 'Data.Aeson.object', then encode it to
a conventional JSON @ByteString@ with
'Data.Aeson.Encode.Pretty.encodePretty' for the pretty-print version.
-}
main :: IO ()
main = do
  opts <- execParser optsParserInfo

  let withMpdOpts = MPD.withMPDEx (optHost opts) (optPort opts) (optPass opts)
  cs <- withMpdOpts MPD.currentSong
  st <- withMpdOpts MPD.status

  -- #TODO update this

  let state :: Maybe String
      state = playbackStateToString <$> getStatusField st MPD.stState
        where
          playbackStateToString Playing = "playing"
          playbackStateToString Paused  = "paused"
          playbackStateToString Stopped = "stopped"

      time = getStatusFieldElement st MPD.stTime
      elapsed = fst <$> time
      duration = snd <$> time

      elapsedPercent :: Maybe Double
      elapsedPercent = readMaybe percentTwoDecimals
        where
          percentTwoDecimals = printf "%.2f" timeToPercent
          timeToPercent = uncurry (/) t * 100
          t = fromMaybe (0,0) time

      volumeSt :: Maybe Int
      volumeSt = fromIntegral <$> getStatusFieldElement st MPD.stVolume

      repeatSt       = getStatusField st MPD.stRepeat
      randomSt       = getStatusField st MPD.stRandom
      singleSt       = getStatusField st MPD.stSingle
      consumeSt      = getStatusField st MPD.stConsume
      bitrate        = getStatusField st MPD.stBitrate
      audioFormat    = getStatusField st MPD.stAudio
      errorSt        = getStatusField st MPD.stError

      updatingDbSt :: Maybe Bool
      updatingDbSt   = (== 1) <$> getStatusFieldElement st MPD.stUpdatingDb

      crossfadeSt :: Maybe Int
      crossfadeSt = fromIntegral <$> getStatusField st MPD.stXFadeWidth

      mixRampDbSt = getStatusField st MPD.stMixRampdB
      mixRampDelay = getStatusField st MPD.stMixRampDelay

  -- positon is an index starting from 0. Id starts from 1
  let pos            = getStatusField st MPD.stSongPos
      nextPos        = getStatusFieldElement st MPD.stNextSongPos
      songId         = getStatusIdInt MPD.stSongID st
      nextId         = getStatusIdInt MPD.stNextSongID st
      playlistLength = getStatusField st MPD.stPlaylistLength

  nextPlaylistSong <- withMpdOpts $ MPD.playlistInfo nextPos
  let filename = maybePathCurrentSong cs
      filenameNext = maybePathNextPlaylistSong nextPlaylistSong

  -- sgTags
  let currentSongTags = getAllTags $ Current cs

  let jCurrentSongTagsObject = objectJson
        [ "artist"                     .=? tagFieldToMaybeString (artist currentSongTags)
        , "artist_sort"                .=? tagFieldToMaybeString (artistSort currentSongTags)
        , "album"                      .=? tagFieldToMaybeString (album currentSongTags)
        , "album_sort"                 .=? tagFieldToMaybeString (albumSort currentSongTags)
        , "album_artist"               .=? tagFieldToMaybeString (albumArtist currentSongTags)
        , "album_artist_sort"          .=? tagFieldToMaybeString (albumArtistSort currentSongTags)
        , "title"                      .=? tagFieldToMaybeString (title currentSongTags)
        , "track"                      .=? tagFieldToMaybeString (track currentSongTags)
        , "name"                       .=? tagFieldToMaybeString (name currentSongTags)
        , "genre"                      .=? tagFieldToMaybeString (genre currentSongTags)
        , "date"                       .=? tagFieldToMaybeString (date currentSongTags)
        , "original_date"              .=? tagFieldToMaybeString (originalDate currentSongTags)
        , "composer"                   .=? tagFieldToMaybeString (composer currentSongTags)
        , "performer"                  .=? tagFieldToMaybeString (performer currentSongTags)
        , "conductor"                  .=? tagFieldToMaybeString (conductor currentSongTags)
        , "work"                       .=? tagFieldToMaybeString (work currentSongTags)
        , "grouping"                   .=? tagFieldToMaybeString (grouping currentSongTags)
        , "comment"                    .=? tagFieldToMaybeString (comment currentSongTags)
        , "disc"                       .=? tagFieldToMaybeString (disc currentSongTags)
        , "label"                      .=? tagFieldToMaybeString (label currentSongTags)
        , "musicbrainz_artistid"       .=? tagFieldToMaybeString (musicbrainz_ArtistId currentSongTags)
        , "musicbrainz_albumid"        .=? tagFieldToMaybeString (musicbrainz_AlbumId currentSongTags)
        , "musicbrainz_albumartistid"  .=? tagFieldToMaybeString (musicbrainz_AlbumartistId currentSongTags)
        , "musicbrainz_trackid"        .=? tagFieldToMaybeString (musicbrainz_TrackId currentSongTags)
        , "musicbrainz_releasetrackid" .=? tagFieldToMaybeString (musicbrainz_ReleasetrackId currentSongTags)
        , "musicbrainz_workid"         .=? tagFieldToMaybeString (musicbrainz_WorkId currentSongTags)
        ]

  -- status
  let jStatus = objectJson
        [ "state"           .=? state
        , "repeat"          .=? repeatSt
        , "random"          .=? randomSt
        , "single"          .=? singleSt
        , "consume"         .=? consumeSt
        , "duration"        .=? duration
        , "elapsed"         .=? elapsed
        , "elapsed_percent" .=? elapsedPercent
        , "volume"          .=? volumeSt
        , "audio_format"    .=? audioFormat
        , "bitrate"         .=? bitrate
        , "crossfade"       .=? crossfadeSt
        , "mixramp_db"      .=? mixRampDbSt
        , "mixramp_delay"   .=? mixRampDelay
        , "updating_db"     .=? updatingDbSt
        , "error"           .=? errorSt
        ]

  -- let jFilename = objectJson [ "file" .=? filename ]

  let jPlaylist = objectJson
        [ "position"      .=? pos  -- current song position
        , "next_position" .=? nextPos
        , "id"            .=? songId  -- current song id
        , "next_id"       .=? nextId
        , "length"        .=? playlistLength
        ]

  let jObject = object [ "filename"      .= filename
                       , "next_filename" .= filenameNext
                       , "playlist"      .= jPlaylist
                       , "status"        .= jStatus
                       , "tags"          .= jCurrentSongTagsObject
                       ]

  let nextSongTags = getAllTags $ Next nextPlaylistSong

  let jNextSongTagsObject = objectJson
        [ "artist"                     .=? tagFieldToMaybeString (artist nextSongTags)
        , "artist_sort"                .=? tagFieldToMaybeString (artistSort nextSongTags)
        , "album"                      .=? tagFieldToMaybeString (album nextSongTags)
        , "album_sort"                 .=? tagFieldToMaybeString (albumSort nextSongTags)
        , "album_artist"               .=? tagFieldToMaybeString (albumArtist nextSongTags)
        , "album_artist_sort"          .=? tagFieldToMaybeString (albumArtistSort nextSongTags)
        , "title"                      .=? tagFieldToMaybeString (title nextSongTags)
        , "track"                      .=? tagFieldToMaybeString (track nextSongTags)
        , "name"                       .=? tagFieldToMaybeString (name nextSongTags)
        , "genre"                      .=? tagFieldToMaybeString (genre nextSongTags)
        , "date"                       .=? tagFieldToMaybeString (date nextSongTags)
        , "original_date"              .=? tagFieldToMaybeString (originalDate nextSongTags)
        , "composer"                   .=? tagFieldToMaybeString (composer nextSongTags)
        , "performer"                  .=? tagFieldToMaybeString (performer nextSongTags)
        , "conductor"                  .=? tagFieldToMaybeString (conductor nextSongTags)
        , "work"                       .=? tagFieldToMaybeString (work nextSongTags)
        , "grouping"                   .=? tagFieldToMaybeString (grouping nextSongTags)
        , "comment"                    .=? tagFieldToMaybeString (comment nextSongTags)
        , "disc"                       .=? tagFieldToMaybeString (disc nextSongTags)
        , "label"                      .=? tagFieldToMaybeString (label nextSongTags)
        , "musicbrainz_artistid"       .=? tagFieldToMaybeString (musicbrainz_ArtistId nextSongTags)
        , "musicbrainz_albumid"        .=? tagFieldToMaybeString (musicbrainz_AlbumId nextSongTags)
        , "musicbrainz_albumartistid"  .=? tagFieldToMaybeString (musicbrainz_AlbumartistId nextSongTags)
        , "musicbrainz_trackid"        .=? tagFieldToMaybeString (musicbrainz_TrackId nextSongTags)
        , "musicbrainz_releasetrackid" .=? tagFieldToMaybeString (musicbrainz_ReleasetrackId nextSongTags)
        , "musicbrainz_workid"         .=? tagFieldToMaybeString (musicbrainz_WorkId nextSongTags)
        ]

  let jNextObject = object [ "next" .= object [ "tags" .= jNextSongTagsObject ] ]

  case optNext opts of
    NoNextSong -> printJson jObject
    OnlyNextSong -> printJson jNextObject
    IncludeNextSong -> do printJson jObject
                          printJson jNextObject
    where
      printJson j = C.putStrLn $ encodePretty' customEncodeConf j

customEncodeConf :: Config
customEncodeConf = defConfig
 { confCompare =
   keyOrder
 [ "title", "name"
 , "artist", "album_artist", "artist_sort", "album_artist_sort"
 , "album", "album_sort"
 , "track", "disc"
 , "date", "original_date"
 , "genre", "composer", "performer", "conductor"
 , "work", "grouping", "label"
 , "comment"
 , "musicbrainz_artistid"
 , "musicbrainz_albumid"
 , "musicbrainz_albumartistid"
 , "musicbrainz_trackid"
 , "musicbrainz_releasetrackid"
 , "musicbrainz_workid"
 -- status
 , "state", "repeat", "random", "single", "consume"
 , "duration", "elapsed", "elapsed_percent"
 , "volume", "audio_format", "bitrate"
 , "crossfade", "mixramp_db", "mixramp_delay"
 , "updating_db"
 , "error"
 -- playlist
 , "position", "next_position", "id", "next_id"
 , "length"
 ]
 }
