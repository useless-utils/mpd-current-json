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
                         , getTagNextSong
                         , maybePathCurrentSong
                         , maybePathNextPlaylistSong
                         , (.=?)
                         , objectJson
                         , getStatusIdInt )

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
  let currentSongTags = getAllTags getTag cs

  let jCurrentSongTagsObject = objectJson
        [ "artist"                     .=? artist currentSongTags
        , "artist_sort"                .=? artistSort currentSongTags
        , "album"                      .=? album currentSongTags
        , "album_sort"                 .=? albumSort currentSongTags
        , "album_artist"               .=? albumArtist currentSongTags
        , "album_artist_sort"          .=? albumArtistSort currentSongTags
        , "title"                      .=? title currentSongTags
        , "track"                      .=? track currentSongTags
        , "name"                       .=? name currentSongTags
        , "genre"                      .=? genre currentSongTags
        , "date"                       .=? date currentSongTags
        , "original_date"              .=? originalDate currentSongTags
        , "composer"                   .=? composer currentSongTags
        , "performer"                  .=? performer currentSongTags
        , "conductor"                  .=? conductor currentSongTags
        , "work"                       .=? work currentSongTags
        , "grouping"                   .=? grouping currentSongTags
        , "comment"                    .=? comment currentSongTags
        , "disc"                       .=? disc currentSongTags
        , "label"                      .=? label currentSongTags
        , "musicbrainz_artistid"       .=? musicbrainz_ArtistId currentSongTags
        , "musicbrainz_albumid"        .=? musicbrainz_AlbumId currentSongTags
        , "musicbrainz_albumartistid"  .=? musicbrainz_AlbumartistId currentSongTags
        , "musicbrainz_trackid"        .=? musicbrainz_TrackId currentSongTags
        , "musicbrainz_releasetrackid" .=? musicbrainz_ReleasetrackId currentSongTags
        , "musicbrainz_workid"         .=? musicbrainz_WorkId currentSongTags
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

  C.putStrLn $ encodePretty' customEncodeConf jObject

  case optNext opts of
    OnlyNextSong -> putStrLn "ONLY NEXT (TEST)"
    IncludeNextSong -> putStrLn "INCLUDE NEXT (TEST)"
    NoNextSong -> putStrLn mempty

  let nextSongTags = getAllTags getTagNextSong nextPlaylistSong

  let jNextSongTagsObject = objectJson
        [ "artist"                     .=? artist nextSongTags
        , "artist_sort"                .=? artistSort nextSongTags
        , "album"                      .=? album nextSongTags
        , "album_sort"                 .=? albumSort nextSongTags
        , "album_artist"               .=? albumArtist nextSongTags
        , "album_artist_sort"          .=? albumArtistSort nextSongTags
        , "title"                      .=? title nextSongTags
        , "track"                      .=? track nextSongTags
        , "name"                       .=? name nextSongTags
        , "genre"                      .=? genre nextSongTags
        , "date"                       .=? date nextSongTags
        , "original_date"              .=? originalDate nextSongTags
        , "composer"                   .=? composer nextSongTags
        , "performer"                  .=? performer nextSongTags
        , "conductor"                  .=? conductor nextSongTags
        , "work"                       .=? work nextSongTags
        , "grouping"                   .=? grouping nextSongTags
        , "comment"                    .=? comment nextSongTags
        , "disc"                       .=? disc nextSongTags
        , "label"                      .=? label nextSongTags
        , "musicbrainz_artistid"       .=? musicbrainz_ArtistId nextSongTags
        , "musicbrainz_albumid"        .=? musicbrainz_AlbumId nextSongTags
        , "musicbrainz_albumartistid"  .=? musicbrainz_AlbumartistId nextSongTags
        , "musicbrainz_trackid"        .=? musicbrainz_TrackId nextSongTags
        , "musicbrainz_releasetrackid" .=? musicbrainz_ReleasetrackId nextSongTags
        , "musicbrainz_workid"         .=? musicbrainz_WorkId nextSongTags
        ]

  let jNextObject = object [ "next" .= object [ "tags" .= jNextSongTagsObject ] ]

  C.putStrLn $ encodePretty' customEncodeConf jNextObject

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




data ExtractedTags = ExtractedTags
  { artist                     :: Maybe String
  , artistSort                 :: Maybe String
  , album                      :: Maybe String
  , albumSort                  :: Maybe String
  , albumArtist                :: Maybe String
  , albumArtistSort            :: Maybe String
  , title                      :: Maybe String
  , track                      :: Maybe String
  , name                       :: Maybe String
  , genre                      :: Maybe String
  , date                       :: Maybe String
  , originalDate               :: Maybe String
  , composer                   :: Maybe String
  , performer                  :: Maybe String
  , conductor                  :: Maybe String
  , work                       :: Maybe String
  , grouping                   :: Maybe String
  , comment                    :: Maybe String
  , disc                       :: Maybe String
  , label                      :: Maybe String
  , musicbrainz_ArtistId       :: Maybe String
  , musicbrainz_AlbumId        :: Maybe String
  , musicbrainz_AlbumartistId  :: Maybe String
  , musicbrainz_TrackId        :: Maybe String
  , musicbrainz_ReleasetrackId :: Maybe String
  , musicbrainz_WorkId         :: Maybe String
  } deriving (Show, Eq)

getAllTags :: (Metadata -> t -> Maybe String) -> t -> ExtractedTags
getAllTags f s = ExtractedTags
  { artist                     = f Artist                     s
  , artistSort                 = f ArtistSort                 s
  , album                      = f Album                      s
  , albumSort                  = f AlbumSort                  s
  , albumArtist                = f AlbumArtist                s
  , albumArtistSort            = f AlbumArtistSort            s
  , title                      = f Title                      s
  , track                      = f Track                      s
  , name                       = f Name                       s
  , genre                      = f Genre                      s
  , date                       = f Date                       s
  , originalDate               = f OriginalDate               s
  , composer                   = f Composer                   s
  , performer                  = f Performer                  s
  , conductor                  = f Conductor                  s
  , work                       = f Work                       s
  , grouping                   = f Grouping                   s
  , comment                    = f Comment                    s
  , disc                       = f Disc                       s
  , label                      = f Label                      s
  , musicbrainz_ArtistId       = f MUSICBRAINZ_ARTISTID       s
  , musicbrainz_AlbumId        = f MUSICBRAINZ_ALBUMID        s
  , musicbrainz_AlbumartistId  = f MUSICBRAINZ_ALBUMARTISTID  s
  , musicbrainz_TrackId        = f MUSICBRAINZ_TRACKID        s
  , musicbrainz_ReleasetrackId = f MUSICBRAINZ_RELEASETRACKID s
  , musicbrainz_WorkId         = f MUSICBRAINZ_WORKID         s
  }


-- example output of
-- ghci> withMpdOpts MPD.status >>= \x -> withMpdOpts $ MPD.playlistInfo $ getStatusIdInt MPD.stNextSongID x
--
-- Right
--   [ Song
--       { sgFilePath = Path "I/Ina Forsman/[2022] All There Is [16bit 44.1kHz FLAC]/06. One Night In Berlin.flac"
--       , sgTags = fromList
--           [ (Artist, [Value "Ina Forsman"])
--           , (ArtistSort, [Value "Forsman, Ina"])
--           , (Album, [Value "All There Is"])
--           , (AlbumArtist, [Value "Ina Forsman"])
--           , (AlbumArtistSort, [Value "Forsman, Ina"])
--           , (Title, [Value "One Night In Berlin"])
--           , (Track, [Value "6"])
--           , (Genre, [Value "R&B, Soul, Funk"])
--           , (Date, [Value "2022-06-25"])
--           , (OriginalDate, [Value "2022-06-25"])
--           , (Composer, [Value "Ina Forsman"])
--           , (Comment,
--               [ Value
--                   "{'Classical Extras': ...} (artists_options)"
--               ])
--           , (Disc, [Value "1"])
--           , (Label, [Value "Jazzhaus Records"])
--           , (MUSICBRAINZ_ARTISTID, [Value "f19fb535-3c91-4be8-8c14-ed06fa079f57"])
--           , (MUSICBRAINZ_ALBUMID, [Value "80083158-e00f-4fa5-913f-6ac452170870"])
--           , (MUSICBRAINZ_ALBUMARTISTID, [Value "f19fb535-3c91-4be8-8c14-ed06fa079f57"])
--           , (MUSICBRAINZ_TRACKID, [Value "86e28f16-047b-4e87-abc3-b7a34ff8f2fd"])
--           , (MUSICBRAINZ_RELEASETRACKID, [Value "94bf46b1-d216-403d-9e07-00084f83f85a"])
--           ]
--       , sgLastModified = Just 2024-03-21 08:24:25 UTC
--       , sgLength = 220
--       , sgId = Just (Id 284)
--       , sgIndex = Just 283
--       }
--   ]
