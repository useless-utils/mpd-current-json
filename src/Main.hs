{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import qualified Network.MPD as MPD
import Network.MPD
       ( Metadata(..), PlaybackState(Stopped, Playing, Paused) )
import Data.Maybe ( catMaybes )
import Data.Aeson ( object, KeyValue((.=)), Value )
import Data.Aeson.Encode.Pretty
       ( defConfig, encodePretty', keyOrder, Config(confCompare) )
import qualified Data.ByteString.Lazy.Char8 as C
import Text.Printf ( printf )
import Options
    ( optsParserInfo, execParser, Opts(optPass, optHost, optPort) )

import Network.MPD.Parse ( getStatusItem
                         , getTag
                         , maybePath
                         , (.=?) )

import Text.Read (readMaybe)
import Data.Aeson.Types (Pair)
{- | Where the program connects to MPD and uses the helper functions to
extract values, organize them into a list of key/value pairs, make
them a 'Data.Aeson.Value' using 'Data.Aeson.object', then encode it to
a conventional JSON @ByteString@ with
'Data.Aeson.Encode.Pretty.encodePretty' for the pretty-print version.
-}
main :: IO ()
main = do
  opts <- execParser optsParserInfo

  cs <- MPD.withMPDEx (optHost opts) (optPort opts) (optPass opts) MPD.currentSong
  st <- MPD.withMPDEx (optHost opts) (optPort opts) (optPass opts) MPD.status

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
      musicbrainz_Artistid       = getTag MUSICBRAINZ_ARTISTID       cs
      musicbrainz_Albumid        = getTag MUSICBRAINZ_ALBUMID        cs
      musicbrainz_Albumartistid  = getTag MUSICBRAINZ_ALBUMARTISTID  cs
      musicbrainz_Trackid        = getTag MUSICBRAINZ_TRACKID        cs
      musicbrainz_Releasetrackid = getTag MUSICBRAINZ_RELEASETRACKID cs
      musicbrainz_Workid         = getTag MUSICBRAINZ_WORKID         cs

  let state :: Maybe String
      state = case getStatusItem st MPD.stState of
                Just ps -> case ps of
                             Playing -> Just "playing"
                             Paused  -> Just "paused"
                             Stopped -> Just "stopped"
                Nothing -> Nothing

      time = getStatusItem st MPD.stTime

      elapsed = case time of
        Just t -> case t of
                    Just (e, _) -> Just e
                    _noTag      -> Nothing
        Nothing -> Nothing

      duration = case time of
        Just t -> case t of
                    Just (_, d) -> Just d
                    _noTag      -> Nothing
        Nothing -> Nothing

      elapsedPercent :: Maybe Double
      elapsedPercent = case time of
        Just t -> case t of
                   Just t1 -> readMaybe $ printf "%.2f" (uncurry (/) t1 * 100)
                   Nothing -> Just 0
        Nothing -> Nothing

      repeatSt       = getStatusItem st MPD.stRepeat
      randomSt       = getStatusItem st MPD.stRandom
      singleSt       = getStatusItem st MPD.stSingle
      consumeSt      = getStatusItem st MPD.stConsume
      pos            = getStatusItem st MPD.stSongPos
      playlistLength = getStatusItem st MPD.stPlaylistLength
      bitrate        = getStatusItem st MPD.stBitrate
      audioFormat    = getStatusItem st MPD.stAudio
      errorSt        = getStatusItem st MPD.stError

  let filename = maybePath cs

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
        , "musicbrainz_artistid"       .=? musicbrainz_Artistid
        , "musicbrainz_albumid"        .=? musicbrainz_Albumid
        , "musicbrainz_albumartistid"  .=? musicbrainz_Albumartistid
        , "musicbrainz_trackid"        .=? musicbrainz_Trackid
        , "musicbrainz_releasetrackid" .=? musicbrainz_Releasetrackid
        , "musicbrainz_workid"         .=? musicbrainz_Workid
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
        , "audio_format"    .=? audioFormat
        , "bitrate"         .=? bitrate
        , "error"           .=? errorSt
        ]

  -- let jFilename = objectJson [ "file" .=? filename ]

  let jPlaylist = objectJson
        [ "position" .=? pos
        , "length"           .=? playlistLength
        ]

  let jObject = object [ "filename" .= filename
                       , "playlist" .= jPlaylist
                       , "status"   .= jStatus
                       , "tags"     .= jTags
                       ]

  C.putStrLn $ encodePretty' customEncodeConf jObject

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
                           , "audio_format", "bitrate"
                           , "error"
                           ]
  }

-- | Helper function for creating an JSON 'Data.Aeson.object' where
-- 'Data.Maybe.catMaybes' won't include items from the '[Maybe Pair]'
-- list that return 'Nothing'.
objectJson :: [Maybe Pair] -> Value
objectJson = object . catMaybes
