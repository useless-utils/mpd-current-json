{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where


import MPD.Current.JSON.Types ( State(..) )
import MPD.Current.JSON.Builders ( currentPlaylist, currentFile )
import MPD.Current.JSON.JSON ()  -- instances
import MPD.Current.JSON.Parse ( getTags )
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
      Config(..),
      Indent(Spaces) )
import Data.ByteString.Lazy.Char8 qualified as C
import System.Exit ( die, exitSuccess )


main :: IO ()
main = do
  opts <- execParser optsParserInfo
  optsExecVersion opts

  let withMpdOpts = MPD.withMPDEx opts.optHost opts.optPort opts.optPass
  response <- withMpdOpts $ do
    cs <- MPD.currentSong
    st <- MPD.status
    let nPos = st.stNextSongPos
    ns <- MPD.playlistInfo nPos
    pure (cs, st, ns)

  case response of
    Right (Just cs, status, [ns]) ->
      -- handle edge case where next song is the same as current
      let opts' = if cs == ns
                  then opts {optNext = NoNextSong}
                  else opts
      in printEncoded opts' cs ns status

    -- something tells me that exceptions are thrown before these get reached
    _ -> die "Couldn't get enough information from MPD."

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
