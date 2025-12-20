module MPD.Current.JSON.Builder
    ( mkStatus
    , mkPlaylist
    , mkFile
    ) where


import MPD.Current.JSON.Types qualified as Current
import Network.MPD qualified as MPD
import Text.Read ( readMaybe )
import Text.Printf ( printf )
import GHC.Num ( integerToInt )

mkStatus :: MPD.Status -> Current.Status
mkStatus st = Current.Status
  { Current.state          = Current.MPDPlaybackState st.stState
  , Current.repeat         = st.stRepeat
  , Current.random         = st.stRandom
  , Current.single         = st.stSingle
  , Current.consume        = st.stConsume
  , Current.duration       = snd <$> st.stTime
  , Current.elapsed        = fst <$> st.stTime
  , Current.elapsedPercent = calcElapsedPercent st.stTime
  , Current.volume         = fromIntegral <$> st.stVolume
  , Current.audioFormat    = Just st.stAudio
  , Current.bitrate        = st.stBitrate
  , Current.crossfade      = Just . integerToInt $ st.stXFadeWidth
  , Current.mixRampDb      = Just st.stMixRampdB
  , Current.mixRampDelay   = Just st.stMixRampDelay
  , Current.updatingDb     = integerToInt <$> st.stUpdatingDb
  , Current.error          = st.stError
  }
  where
    calcElapsedPercent :: Maybe (MPD.FractionalSeconds, MPD.FractionalSeconds) -> Maybe Double
    calcElapsedPercent Nothing = Nothing
    calcElapsedPercent (Just (elapsed, duration)) = do
      let elapsedPercent = (elapsed / duration) * 100
      if duration > 0
        then readMaybe $ printf "%02.2f" elapsedPercent :: Maybe Double
        else Nothing

mkPlaylist :: MPD.Status -> Current.Playlist
mkPlaylist st = Current.Playlist
  { Current.position     = st.stSongPos
  , Current.nextPosition = st.stNextSongPos
  , Current.id           = Current.MPDId <$> st.stSongID
  , Current.nextId       = Current.MPDId <$> st.stNextSongID
  , Current.length       = fromIntegral st.stPlaylistLength
  }

mkFile :: MPD.Song -> MPD.Song -> Current.File
mkFile cs ns = Current.File
  { Current.currentFile = if null $ MPD.toString cs.sgFilePath
                          then Nothing
                          else Just $ Current.MPDPath cs.sgFilePath
  , Current.nextFile    = if null $ MPD.toString ns.sgFilePath
                          then Nothing
                          else Just $ Current.MPDPath ns.sgFilePath
  }
