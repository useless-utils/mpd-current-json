{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}


module MPD.Current.JSON.Parse where

import MPD.Current.JSON.Types
    ( Tags(..), TagField(..) )

import Data.Maybe ( listToMaybe, fromMaybe )
import           Network.MPD
  ( Metadata(..), Song, PlaybackState(Stopped, Playing, Paused), Response )
import qualified Network.MPD as MPD


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

getTag tag song = tagSingleOrList (MPD.sgGetTag tag song)
  where
    tagSingleOrList :: Maybe [MPD.Value] -> TagField
    tagSingleOrList val
      | fmap length val == Just 1
      = SingleTagField
        $ singleValueToString
        $ listToMaybe
        $ fromMaybe [] val
      | fmap length val > Just 1
      = MultiTagField
        $ multiValueToString val
      | otherwise = SingleTagField Nothing


{- | Convert 'Network.MPD.Value' to @String@ within a @Maybe@ context.

'MPD.sgGetTag' returns a @Maybe [Value]@. [libmpd](Network.MPD) also
provides 'Network.MPD.toString' that can also, along with @ByteString@
and @Text@, convert a 'Network.MPD.Value' to a @String@.
-}
singleValueToString :: Maybe MPD.Value -> Maybe String
singleValueToString (Just x) = Just (MPD.toString x)
singleValueToString Nothing = Nothing

{- | Same as 'singleValueToString' but converts all @Value@s in the
multi-value-tag list to @String@ and returns the list.

`reverse' is used here because multi-value tags are returned in
reverse order by [libmpd](Network.MPD), e.g. if a song has a
multi-value @artist@ tag that contains "Artist1; Artist2; Artist3",
the returned value of 'Network.MPD.Song.sgTags' from
`Network.MPD.playlistInfo' @-> [Song]@ (which is a way of fetching the
next song) would be @["Artist3", "Artist2", "Artist1"]@.
-}
multiValueToString :: Maybe [MPD.Value] -> Maybe [String]
multiValueToString (Just x) = Just $ reverse $ map MPD.toString x
multiValueToString Nothing = Nothing

fromResponseStatusField :: MPD.Response MPD.Status -> (MPD.Status -> a) -> Maybe a
fromResponseStatusField (Right st) f = Just (f st)
fromResponseStatusField _ _ = Nothing

{- | Go a level deeper than `getStatusField'. For nested @Maybe a@
fields from 'Network.MPD.Status'.

==== __Example__:

@
ghci> import qualified Network.MPD as MPD
ghci> st <- MPD.withMPD MPD.status
ghci> fromResponseStatusFieldElement st MPD.stVolume
@
Just 100
-}
fromResponseStatusFieldElement :: MPD.Response MPD.Status -> (MPD.Status -> Maybe a) -> Maybe a
fromResponseStatusFieldElement status item =
  fromMaybe Nothing $ fromResponseStatusField status item
