{-# LANGUAGE ImportQualifiedPost #-}

module MPD.Current.JSON.Builders where

import MPD.Current.JSON.Types qualified as Current
import Network.MPD qualified as MPD

import Data.ByteString qualified as B

-- currentMPDState is in Main

-- buildPlaylistInfo :: MPD.Status -> PlaylistInfo
currentPlaylist st = Current.Playlist
  { Current.position     = st.stSongPos
  , Current.nextPosition = st.stNextSongPos
  , Current.id           = st.stSongID
  , Current.nextId       = st.stNextSongID
  , Current.length       = fromIntegral st.stPlaylistLength
  }

-- buildFileInfo :: MPD.Song -> MPD.Song -> FileInfo
currentFile cs ns = Current.File
  { Current.currentFile = if null $ MPD.toString cs.sgFilePath
                          then Nothing
                          else Just cs.sgFilePath
  , Current.nextFile    = if null $ MPD.toString ns.sgFilePath
                          then Nothing
                          else Just ns.sgFilePath
  }
