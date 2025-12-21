{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}


module MPD.Current.JSON.Parse
    ( getTags
    ) where

import MPD.Current.JSON.Types ( Tags(..), TagField(..) )
import Network.MPD ( Metadata(..) )
import qualified Network.MPD as MPD


-- | Builder for `Tags' from `Network.MPD.Song' that assigns `TagField' values
-- based on them being a single string or multi-value array.
getTags :: MPD.Song -> Tags
getTags song = Tags
  { artist                    = getTag Artist                     song
  , artistSort                = getTag ArtistSort                 song
  , album                     = getTag Album                      song
  , albumSort                 = getTag AlbumSort                  song
  , albumArtist               = getTag AlbumArtist                song
  , albumArtistSort           = getTag AlbumArtistSort            song
  , title                     = getTag Title                      song
  , track                     = getTag Track                      song
  , name                      = getTag Name                       song
  , genre                     = getTag Genre                      song
  , date                      = getTag Date                       song
  , originalDate              = getTag OriginalDate               song
  , composer                  = getTag Composer                   song
  , performer                 = getTag Performer                  song
  , conductor                 = getTag Conductor                  song
  , work                      = getTag Work                       song
  , grouping                  = getTag Grouping                   song
  , comment                   = getTag Comment                    song
  , disc                      = getTag Disc                       song
  , label                     = getTag Label                      song
  , musicbrainzArtistId       = getTag MUSICBRAINZ_ARTISTID       song
  , musicbrainzAlbumId        = getTag MUSICBRAINZ_ALBUMID        song
  , musicbrainzAlbumartistId  = getTag MUSICBRAINZ_ALBUMARTISTID  song
  , musicbrainzTrackId        = getTag MUSICBRAINZ_TRACKID        song
  , musicbrainzReleasetrackId = getTag MUSICBRAINZ_RELEASETRACKID song
  , musicbrainzWorkId         = getTag MUSICBRAINZ_WORKID         song
  }


-- | Parse single or multi `Network.MPD.Value'
getTag :: Metadata -> MPD.Song -> Maybe TagField
getTag tag song = tagSingleOrList (MPD.sgGetTag tag song)
  where
    tagSingleOrList :: Maybe [MPD.Value] -> Maybe TagField
    tagSingleOrList val = case val of
      Just [v] -> Just . SingleTagField $ MPD.toString v
      Just v -> Just . MultiTagField $ map MPD.toString v
      Nothing -> Nothing
