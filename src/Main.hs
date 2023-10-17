{-# LANGUAGE OverloadedStrings #-}

module Main ( main,
              getStatusItem,
              getTag,
              processSong,
              headMay,
              valueToStringMay,
              (.=?) ) where

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

{- | Extract a field from the returned MPD.Status data record.

This takes an @Either@ 'Network.MPD.MPDError' 'Network.MPD.Status'
value and a field label function @f@ as arguments. It returns @Just
(f st)@ if the input status is @Right st@, where @st@ is the
'Network.MPD.Status' value. This function helps to extract a
specific field from the @MPD.Status@ data record by providing the
corresponding field label function.  If the input status "@st@" is
not @Right st@, indicating an error, or the field label function is
not applicable, it returns @Nothing@.
-}
getStatusItem :: Either MPD.MPDError MPD.Status -> (MPD.Status -> a) -> Maybe a
getStatusItem (Right st) f = Just (f st)
getStatusItem _ _ = Nothing

{- | @Either@ check for the returned value of 'Network.MPD.currentSong',
then call 'processSong' or return @Nothing@.
-}
getTag :: Metadata -> Either a (Maybe Song) -> Maybe String
getTag t c =
  case c of
    Left _ -> Nothing
    Right song -> processSong t song

{- | Use 'Network.MPD.sgGetTag' to extract a @tag@ from a @song@, safely
get only the head item of the returned @Maybe@ list, then safely
convert it to a string.
-}
processSong :: Metadata -> Maybe Song -> Maybe String
processSong _ Nothing = Nothing
processSong tag (Just song) = do
  let tagVal = MPD.sgGetTag tag song
  valueToStringMay =<< (headMay =<< tagVal)

{- | Safely get the head of a list. Same as 'Safe.headMay'.
-}
headMay :: [a] -> Maybe a
headMay []    = Nothing
headMay (x:_) = Just x

{- | Convert 'Network.MPD.Value' to @String@ within a @Maybe@ context.

This @Value@ is from 'Network.MPD' and is basically the same as a
@String@ but used internally to store metadata values.

__Example__:

@
processSong :: Metadata -> Maybe Song -> Maybe String
processSong _ Nothing = Nothing
processSong tag (Just song) = do
  let tagVal = MPD.sgGetTag tag song
  valueToStringMay =<< (headMay =<< tagVal)
@

'MPD.sgGetTag' returns a @Maybe [Value]@. 'Network.MPD' also provides
'Network.MPD.toString' that can convert, along other types, a
'Network.MPD.Value' to a @String@.
-}
valueToStringMay :: MPD.Value -> Maybe String
valueToStringMay x = Just (MPD.toString x)

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
(.=?) :: (KeyValue a, ToJSON v) => Key -> Maybe v -> Maybe a
key .=? Just value = Just (key .= value)
_   .=? Nothing    = Nothing
