{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where


import MPD.Current.JSON.Types ( State(..) )
import MPD.Current.JSON.Builders ( currentPlaylist, currentFile )
import MPD.Current.JSON.JSON ()  -- instances
import MPD.Current.JSON.Parse
    ( getTags, fromResponseStatusFieldElement )
import Network.MPD qualified as MPD
import Options
    ( execParser,
      NextSongFlag(IncludeNextSong, NoNextSong, OnlyNextSong),
      Opts(..),
      optsParserInfo )
import Version ( versionStr )

import Data.Aeson ( object, KeyValue((.=)), ToJSON(toJSON) )
import Data.Aeson.Encode.Pretty
    ( defConfig,
      encodePretty',
      keyOrder,
      Config(confIndent, confCompare),
      Indent(Spaces) )
import Data.ByteString.Lazy.Char8 qualified as C
import System.Exit ( die, exitFailure, exitSuccess )


{- | Where the program connects to MPD and uses the helper functions to
extract values, organize them into a list of key/value pairs, make
them a 'Data.Aeson.Value' using 'Data.Aeson.object', then encode it to
a conventional JSON @ByteString@ with
'Data.Aeson.Encode.Pretty.encodePretty' for the pretty-print version.
-}
main :: IO ()
main = do
  opts <- execParser optsParserInfo
  optsExecVersion opts

  let withMpdOpts = MPD.withMPDEx opts.optHost opts.optPort opts.optPass
  responseCurrentSong <- withMpdOpts MPD.currentSong
  responseStatus <- withMpdOpts MPD.status
  let nextPos = fromResponseStatusFieldElement responseStatus MPD.stNextSongPos
  responseNextSong <- withMpdOpts $ MPD.playlistInfo nextPos

  case (responseCurrentSong, responseStatus, responseNextSong) of
    (Right (Just cs), Right status, Right [ns]) ->
      -- handle edge case where next song is the same as current
      let opts' = if cs == ns
                  then opts {optNext = NoNextSong}
                  else opts
      in printEncoded opts' cs ns status

    -- something tells me that exceptions are thrown before these get reached
    (Left cs, _, _) -> do
      putStrLn "[MPD-ERROR] Couldn't get current song."
      print cs
      exitFailure
    (_, Left status, _) -> do
      putStrLn "[MPD-ERROR] Couldn't get MPD status info."
      print status
      exitFailure
    (Right Nothing, _, _) -> do
      putStrLn "No current song."
      exitFailure
    (_, _, _) -> die "Couldn't get enough information from MPD."

printEncoded :: Opts -> MPD.Song -> MPD.Song -> MPD.Status -> IO ()
printEncoded opts cs ns status =
  let mpdState = currentMPDState opts cs ns status
      finalJson = case opts.optNext of
                    OnlyNextSong -> object ["tags" .= mpdState.mpdNextTags]
                    _ -> toJSON mpdState
  in C.putStrLn $ encodePretty' customEncodeConf finalJson

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

-- | Main builder function that creates the complete state
currentMPDState :: Opts -> MPD.Song -> MPD.Song -> MPD.Status -> State
currentMPDState opts currentSong nextSong status =
  State
  { mpdFiles = currentFile currentSong nextSong
  , mpdStatus = status
  , mpdPlaylist = currentPlaylist status
  , mpdTags = getTags currentSong
  , mpdNextTags = case opts.optNext of
      NoNextSong      -> Nothing
      OnlyNextSong    -> Just (getTags nextSong)
      IncludeNextSong -> Just (getTags nextSong)
  }

optsExecVersion :: Opts -> IO ()
optsExecVersion opts
  | opts.optVersion = do putStrLn versionStr
                         exitSuccess
  | otherwise = pure ()
