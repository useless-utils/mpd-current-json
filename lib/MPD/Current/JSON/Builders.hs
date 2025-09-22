{-# LANGUAGE ImportQualifiedPost #-}

module MPD.Current.JSON.Builders where

import MPD.Current.JSON.Types
import MPD.Current.JSON.Parse

import Network.MPD qualified as MPD
import Data.Maybe
import Text.Printf
import Text.Read

-- | Smart constructors that handle the MPD -> Record conversion
buildPlayerStatus :: MPD.Response MPD.Status -> Status
buildPlayerStatus st = Status
  { psState          = playbackStateToString <$> getStatusField st MPD.stState
  , psRepeat         = fromMaybe False (getStatusField st MPD.stRepeat)
  , psRandom         = fromMaybe False (getStatusField st MPD.stRandom)
  , psSingle         = fromMaybe False (getStatusField st MPD.stSingle)
  , psConsume        = fromMaybe False (getStatusField st MPD.stConsume)
  , psDuration       = snd <$> getStatusFieldElement st MPD.stTime
  , psElapsed        = fst <$> getStatusFieldElement st MPD.stTime
  , psElapsedPercent = calcElapsedPercent st
  , psVolume         = fromIntegral <$> getStatusFieldElement st MPD.stVolume
  , psAudioFormat    = getStatusField st MPD.stAudio
  , psBitrate        = getStatusFieldElement st MPD.stBitrate
  , psCrossfade      = fromIntegral <$> getStatusField st MPD.stXFadeWidth
  , psMixRampDb      = getStatusField st MPD.stMixRampdB
  , psMixRampDelay   = getStatusField st MPD.stMixRampDelay
  , psUpdatingDb     = (== 1) <$> getStatusFieldElement st MPD.stUpdatingDb
  , psError          = getStatusFieldElement st MPD.stError
  }
  where
    playbackStateToString MPD.Playing = "playing"
    playbackStateToString MPD.Paused  = "paused"
    playbackStateToString MPD.Stopped = "stopped"

    calcElapsedPercent status = do
      time <- getStatusField status MPD.stTime
      let (elapsed, duration) = fromMaybe (0, 0) time
      let elapsedPercent = (elapsed / duration) * 100
      if duration > 0
        then readMaybe $ printf "%02.2f" elapsedPercent
        else Nothing

buildPlaylistInfo :: MPD.Response MPD.Status -> PlaylistInfo
buildPlaylistInfo st = PlaylistInfo
  { piPosition     = getStatusFieldElement st MPD.stSongPos
  , piNextPosition = getStatusFieldElement st MPD.stNextSongPos
  , piId           = getStatusIdInt MPD.stSongID st
  , piNextId       = getStatusIdInt MPD.stNextSongID st
  , piLength       = fromIntegral <$> getStatusField st MPD.stPlaylistLength
  }

buildFileInfo :: MPD.Response (Maybe MPD.Song) -> MPD.Response [MPD.Song] -> FileInfo
buildFileInfo currentSong nextSong = FileInfo
  { fiCurrentFile = maybePathCurrentSong currentSong
  , fiNextFile    = maybePathNextPlaylistSong nextSong
  }

