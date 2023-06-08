{-# LANGUAGE OverloadedStrings #-}

import qualified Network.MPD as MPD
import Network.MPD
    ( Metadata(..), Song, PlaybackState(Stopped, Playing, Paused) )
import Data.Maybe ( catMaybes )
import Data.Aeson ( object, Key, KeyValue(..), ToJSON )
import Data.Aeson.Encode.Pretty ( encodePretty )
import qualified Data.ByteString.Lazy.Char8 as C
import Text.Printf ( printf )

main :: IO ()
main = do
  currSong <- MPD.withMPD MPD.currentSong
  currStatus <- MPD.withMPD MPD.status
  let st = currStatus
  let cs = currSong
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
      musicbrainz_artistid       = getTag MUSICBRAINZ_ARTISTID       cs
      musicbrainz_albumid        = getTag MUSICBRAINZ_ALBUMID        cs
      musicbrainz_albumartistid  = getTag MUSICBRAINZ_ALBUMARTISTID  cs
      musicbrainz_trackid        = getTag MUSICBRAINZ_TRACKID        cs
      musicbrainz_releasetrackid = getTag MUSICBRAINZ_RELEASETRACKID cs
      musicbrainz_workid         = getTag MUSICBRAINZ_WORKID         cs

  let state :: Maybe String
      state = case getStatusItem st MPD.stState of
                Just ps -> case ps of
                             Playing -> Just "play"  -- same as mpc
                             Paused -> Just "pause"  -- same as mpc
                             Stopped -> Just "stopped"
                Nothing -> Nothing

      repeat' = getStatusItem st MPD.stRepeat
      time = getStatusItem st MPD.stTime

      elapsed = case time of
        Just t -> case t of
                    Just (e, _) -> Just e
                    _ -> Nothing
        Nothing -> Nothing

      duration = case time of
        Just t -> case t of
                    Just (_, d) -> Just d
                    _ -> Nothing
        Nothing -> Nothing

      elapsed_percent :: Maybe Double
      elapsed_percent = case time of
        Just t -> case t of
                    Just t1 -> Just (read $ printf "%.2f" (uncurry (/) t1 * 100))
                    Nothing -> Just 0
        Nothing -> Nothing

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
        , "musicbrainz_artistid"       .=? musicbrainz_artistid
        , "musicbrainz_albumid"        .=? musicbrainz_albumid
        , "musicbrainz_albumartistid"  .=? musicbrainz_albumartistid
        , "musicbrainz_trackid"        .=? musicbrainz_trackid
        , "musicbrainz_releasetrackid" .=? musicbrainz_releasetrackid
        , "musicbrainz_workid"         .=? musicbrainz_workid
        ]

  let jStatus = object . catMaybes $
        [ "state"                      .=? state
        , "repeat"                     .=? repeat'
        , "elapsed" .=? elapsed
        , "duration" .=? duration
        , "elapsed_pencent" .=? elapsed_percent
        ]

  let jObject = object [ "tags" .= jTags
                   , "status" .= jStatus ]
  C.putStrLn $ encodePretty jObject
  -- putStrLn $ show state

getTag :: Metadata -> Either a (Maybe Song) -> Maybe String
getTag t c =
  case c of
    Left _ -> Nothing
    Right song -> processSong t song

getStatusItem :: Either MPD.MPDError MPD.Status -> (MPD.Status -> a) -> Maybe a
getStatusItem (Right st) f = Just (f st)
getStatusItem _ _ = Nothing

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

