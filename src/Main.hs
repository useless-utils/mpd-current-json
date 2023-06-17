{-# LANGUAGE OverloadedStrings #-}

import qualified Network.MPD as MPD
import Network.MPD
    ( Metadata(..), Song, PlaybackState(Stopped, Playing, Paused) )
import Data.Maybe ( catMaybes )
import Data.Aeson ( object, Key, KeyValue(..), ToJSON )
import Data.Aeson.Encode.Pretty ( encodePretty )
import qualified Data.ByteString.Lazy.Char8 as C
import Text.Printf ( printf )
import Options
    ( optsParserInfo, execParser, Opts(optPass, optHost, optPort) )
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
                             Playing -> Just "play"  -- same as mpc
                             Paused  -> Just "pause"  -- same as mpc
                             Stopped -> Just "stopped"
                Nothing -> Nothing

      time = getStatusItem st MPD.stTime

      elapsed = case time of
        Just t -> case t of
                    Just (e, _) -> Just e
                    _           -> Nothing
        Nothing -> Nothing

      duration = case time of
        Just t -> case t of
                    Just (_, d) -> Just d
                    _           -> Nothing
        Nothing -> Nothing

      elapsedPercent :: Maybe Double
      elapsedPercent = case time of
        Just t -> case t of
                    Just t1 -> Just (read $ printf "%.2f" (uncurry (/) t1 * 100))
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

  -- sgTags
  let jTags = object . catMaybes $
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
  let jStatus = object . catMaybes $
        [ "state"           .=? state
        , "repeat"          .=? repeatSt
        , "elapsed"         .=? elapsed
        , "duration"        .=? duration
        , "elapsed_percent" .=? elapsedPercent
        , "random"          .=? randomSt
        , "single"          .=? singleSt
        , "consume"         .=? consumeSt
        , "song_position"   .=? pos
        , "playlist_length" .=? playlistLength
        , "bitrate"         .=? bitrate
        , "audio_format"    .=? audioFormat
        , "error"           .=? errorSt
        ]

  let jObject = object [ "tags"   .= jTags
                       , "status" .= jStatus ]

  C.putStrLn $ encodePretty jObject

getStatusItem :: Either MPD.MPDError MPD.Status -> (MPD.Status -> a) -> Maybe a
getStatusItem (Right st) f = Just (f st)
getStatusItem _ _ = Nothing

getTag :: Metadata -> Either a (Maybe Song) -> Maybe String
getTag t c =
  case c of
    Left _ -> Nothing
    Right song -> processSong t song

processSong :: Metadata -> Maybe Song -> Maybe String
processSong _ Nothing = Nothing
processSong tag (Just song) = do
  let tagVal = MPD.sgGetTag tag song
  valueToStringMay =<< (headMay =<< tagVal)

-- Utility function to safely get the head of a list
headMay :: [a] -> Maybe a
headMay []    = Nothing
headMay (x:_) = Just x

-- Utility function to convert Value to String within a Maybe context
valueToStringMay :: MPD.Value -> Maybe String
valueToStringMay x = Just (MPD.toString x)

-- Utility function to define optional fields
(.=?) :: (KeyValue a, ToJSON v) => Key -> Maybe v -> Maybe a
key .=? Just value = Just (key .= value)
_   .=? Nothing    = Nothing
