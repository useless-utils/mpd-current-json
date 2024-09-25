{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import qualified Network.MPD as MPD
import Network.MPD
       ( Metadata(..), PlaybackState(Stopped, Playing, Paused) )
import Data.Aeson ( object, KeyValue((.=)) )
import Data.Aeson.Encode.Pretty
       ( defConfig, encodePretty', keyOrder, Config(confCompare) )
import qualified Data.ByteString.Lazy.Char8 as C
import Text.Printf ( printf )
import Options
       ( optsParserInfo, execParser, Opts(..), NextSong(..) )

import Network.MPD.Parse ( getStatusField
                         , getStatusFieldElement
                         , getTag
                         , maybePathCurrentSong
                         , maybePathNextPlaylistSong
                         , (.=?)
                         , objectJson
                         , getStatusIdInt )

import Text.Read (readMaybe)
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

  let artist                     = getTag Artist                     cs
      artistSort                 = getTag ArtistSort                 cs
      album                      = getTag Album                      cs
      albumSort                  = getTag AlbumSort                  cs
      albumArtist                = getTag AlbumArtist                cs
      albumArtistSort            = getTag AlbumArtistSort            cs
      title                      = getTag Title                      cs
      track                      = getTag Track                      cs
      name                       = getTag Name                       cs
      genre                      = getTag Genre                      cs
      date                       = getTag Date                       cs
      originalDate               = getTag OriginalDate               cs
      composer                   = getTag Composer                   cs
      performer                  = getTag Performer                  cs
      conductor                  = getTag Conductor                  cs
      work                       = getTag Work                       cs
      grouping                   = getTag Grouping                   cs
      comment                    = getTag Comment                    cs
      disc                       = getTag Disc                       cs
      label                      = getTag Label                      cs
      musicbrainz_ArtistId       = getTag MUSICBRAINZ_ARTISTID       cs
      musicbrainz_AlbumId        = getTag MUSICBRAINZ_ALBUMID        cs
      musicbrainz_AlbumartistId  = getTag MUSICBRAINZ_ALBUMARTISTID  cs
      musicbrainz_TrackId        = getTag MUSICBRAINZ_TRACKID        cs
      musicbrainz_ReleasetrackId = getTag MUSICBRAINZ_RELEASETRACKID cs
      musicbrainz_WorkId         = getTag MUSICBRAINZ_WORKID         cs

  let state :: Maybe String
      state = case getStatusField st MPD.stState of
                Just ps -> case ps of
                             Playing -> Just "playing"
                             Paused  -> Just "paused"
                             Stopped -> Just "stopped"
                Nothing -> Nothing

      time = getStatusFieldElement st MPD.stTime
      elapsed = fst <$> time
      duration = snd <$> time

      elapsedPercent :: Maybe Double
      elapsedPercent = case time of
        Just t1 -> readMaybe $ printf "%.2f" (uncurry (/) t1 * 100)
        Nothing -> Just 0.0

      volumeSt :: Maybe Int
      volumeSt = fromIntegral <$> getStatusFieldElement st MPD.stVolume

      repeatSt       = getStatusField st MPD.stRepeat
      randomSt       = getStatusField st MPD.stRandom
      singleSt       = getStatusField st MPD.stSingle
      consumeSt      = getStatusField st MPD.stConsume
      bitrate        = getStatusField st MPD.stBitrate
      audioFormat    = getStatusField st MPD.stAudio
      errorSt        = getStatusField st MPD.stError

      updatingDbSt   = isJust updatingDbMaybe
        where
          updatingDbMaybe = getStatusFieldElement st MPD.stUpdatingDb -- "Nothing" or "Just 1"
          isJust Nothing = Nothing
          isJust _       = Just True

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

  nextPlaylistSong <- withMpdOpts $ MPD.playlistInfo nextId
  let filename = maybePathCurrentSong cs
      filenameNext = maybePathNextPlaylistSong nextPlaylistSong

  -- sgTags
  let jTags = objectJson
        [ "artist"                     .=? artist
        , "artist_sort"                .=? artistSort
        , "album"                      .=? album
        , "album_sort"                 .=? albumSort
        , "album_artist"               .=? albumArtist
        , "album_artist_sort"          .=? albumArtistSort
        , "title"                      .=? title
        , "track"                      .=? track
        , "name"                       .=? name
        , "genre"                      .=? genre
        , "date"                       .=? date
        , "original_date"              .=? originalDate
        , "composer"                   .=? composer
        , "performer"                  .=? performer
        , "conductor"                  .=? conductor
        , "work"                       .=? work
        , "grouping"                   .=? grouping
        , "comment"                    .=? comment
        , "disc"                       .=? disc
        , "label"                      .=? label
        , "musicbrainz_artistid"       .=? musicbrainz_ArtistId
        , "musicbrainz_albumid"        .=? musicbrainz_AlbumId
        , "musicbrainz_albumartistid"  .=? musicbrainz_AlbumartistId
        , "musicbrainz_trackid"        .=? musicbrainz_TrackId
        , "musicbrainz_releasetrackid" .=? musicbrainz_ReleasetrackId
        , "musicbrainz_workid"         .=? musicbrainz_WorkId
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
                       , "tags"          .= jTags
                       ]

  C.putStrLn $ encodePretty' customEncodeConf jObject

  case optNext opts of
    OnlyNextSong -> putStrLn "ONLY NEXT (TEST)"
    IncludeNextSong -> putStrLn "INCLUDE NEXT (TEST)"
    NoNextSong -> putStrLn mempty

customEncodeConf :: Config
customEncodeConf = defConfig
  { confCompare = keyOrder [ "title", "name"
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
