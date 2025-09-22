{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where


import MPD.Current.JSON.Builders
    ( buildPlayerStatus, buildPlaylistInfo, buildFileInfo )
import           MPD.Current.JSON.JSON ()  -- instances
import MPD.Current.JSON.Types ( MPDState(..) )
import MPD.Current.JSON.Parse
    ( SongQuery(QueryNext, QueryCurrent),
      getAllTags,
      getStatusFieldElement )
import qualified Network.MPD as MPD
import Options
    ( Opts(optVersion, optNext, optHost, optPort, optPass),
      NextSongFlag(IncludeNextSong, NoNextSong, OnlyNextSong),
      optsParserInfo,
      execParser )

import Data.Aeson ( object, KeyValue((.=)), ToJSON(toJSON) )
import Data.Aeson.Encode.Pretty
    ( defConfig,
      encodePretty',
      keyOrder,
      Config(confIndent, confCompare),
      Indent(Spaces) )
import qualified Data.ByteString.Lazy.Char8 as C
import System.Exit ( exitSuccess )
import Version ( versionStr )


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

  currentSong <- withMpdOpts MPD.currentSong
  status <- withMpdOpts MPD.status
  let nextPos = getStatusFieldElement status MPD.stNextSongPos
  nextSong <- withMpdOpts $ MPD.playlistInfo nextPos

  let mpdState = buildMPDState opts currentSong nextSong status

  let finalJson = case optNext opts of
        OnlyNextSong -> object ["tags" .= mpdNextTags mpdState]
        _ -> toJSON mpdState

  C.putStrLn $ encodePretty' customEncodeConf finalJson

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
buildMPDState :: Opts -> MPD.Response (Maybe MPD.Song) -> MPD.Response [MPD.Song]
              -> MPD.Response MPD.Status -> MPDState
buildMPDState opts currentSong nextSong status = MPDState
  { mpdFiles = buildFileInfo currentSong nextSong
  , mpdStatus = buildPlayerStatus status
  , mpdPlaylist = buildPlaylistInfo status
  , mpdTags = getAllTags QueryCurrent currentSong
  , mpdNextTags = case optNext opts of
      NoNextSong -> Nothing
      OnlyNextSong -> Just (getAllTags QueryNext nextSong)
      IncludeNextSong -> Just (getAllTags QueryNext nextSong)
  }

optsExecVersion :: Opts -> IO ()
optsExecVersion opts | opts.optVersion = do putStrLn versionStr
                                            exitSuccess
                     | otherwise = pure ()
