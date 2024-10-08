{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import qualified Network.MPD as MPD
import Network.MPD ( PlaybackState(Stopped, Playing, Paused) )

import Network.MPD.Parse
    ( getAllTags,
      getStatusField,
      getStatusFieldElement,
      getStatusIdInt,
      maybePathCurrentSong,
      maybePathNextPlaylistSong,
      SongCurrentOrNext(..) )
import Network.MPD.JSON ( objectMaybes, jsonSongTags, (.=?) )
import Options
       ( optsParserInfo, execParser, Opts(..), NextSongFlag(..) )

import Data.Aeson ( object, KeyValue((.=)) )
import Data.Aeson.Encode.Pretty
       ( defConfig, encodePretty', keyOrder, Config(..), Indent(..) )
import qualified Data.ByteString.Lazy.Char8 as C
import Text.Printf ( printf )

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
  currentSong <- withMpdOpts MPD.currentSong
  st <- withMpdOpts MPD.status

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

  nextSong <- withMpdOpts $ MPD.playlistInfo nextPos
  let filename = maybePathCurrentSong currentSong
      filenameNext = maybePathNextPlaylistSong nextSong

  -- sgTags
  let jsonCurrentSongTags = jsonSongTags $ getAllTags $ Current currentSong
      jsonNextSongTags = jsonSongTags $ getAllTags $ Next nextSong

  -- status
  let jsonStatus = objectMaybes
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

  -- let jFilename = objectMaybes [ "file" .=? filename ]

  let jsonPlaylist = objectMaybes
        [ "position"      .=? pos
        , "next_position" .=? nextPos
        , "id"            .=? songId
        , "next_id"       .=? nextId
        , "length"        .=? playlistLength
        ]

  let jsonBaseObject tags = object
                $ [ "filename"      .= filename
                  , "next_filename" .= filenameNext
                  , "playlist"      .= jsonPlaylist
                  , "status"        .= jsonStatus
                  ] ++ tags

  let printJson tags = C.putStrLn
                       $ encodePretty' customEncodeConf
                       $ jsonBaseObject tags

  case optNext opts of
    NoNextSong -> printJson [ "tags" .= jsonCurrentSongTags ]

    OnlyNextSong -> printJson [ "tags" .= jsonNextSongTags ]
    IncludeNextSong -> printJson [ "tags" .= jsonCurrentSongTags
                                 , "next" .= object [ "tags" .= jsonNextSongTags ] ]

customEncodeConf :: Config
customEncodeConf = defConfig
 { confCompare =
     keyOrder
     -- top level labels
     [ "filename", "next_filename", "status", "playlist", "tags", "next"
     -- tags
     , "title", "name"
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
     , "id", "next_id", "position", "next_position"
     , "length"
     ]
 , confIndent = Spaces 2
 }
