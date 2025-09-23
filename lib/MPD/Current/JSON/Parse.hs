{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}


module MPD.Current.JSON.Parse where

import MPD.Current.JSON.Types

import           Data.Kind ( Type )
import           Data.Maybe
import           GHC.Generics
import           Network.MPD
  ( Metadata(..), Song, PlaybackState(Stopped, Playing, Paused), Response )
import qualified Network.MPD as MPD


data WhichSong = Current | Next
type family SongData (s :: WhichSong) :: Type where
  SongData 'Current = Maybe Song
  SongData 'Next = [Song]

data SongQuery (s :: WhichSong) where
  QueryCurrent :: SongQuery 'Current
  QueryNext :: SongQuery 'Next

type CurrentSong = Response (SongData 'Current)
type NextSong = Response (SongData 'Next)


getTags :: SongQuery s -> Response (SongData s) -> Tags
getTags query s            = Tags
  { artist                    = getTag query Artist                     s
  , artistSort                = getTag query ArtistSort                 s
  , album                     = getTag query Album                      s
  , albumSort                 = getTag query AlbumSort                  s
  , albumArtist               = getTag query AlbumArtist                s
  , albumArtistSort           = getTag query AlbumArtistSort            s
  , title                     = getTag query Title                      s
  , track                     = getTag query Track                      s
  , name                      = getTag query Name                       s
  , genre                     = getTag query Genre                      s
  , date                      = getTag query Date                       s
  , originalDate              = getTag query OriginalDate               s
  , composer                  = getTag query Composer                   s
  , performer                 = getTag query Performer                  s
  , conductor                 = getTag query Conductor                  s
  , work                      = getTag query Work                       s
  , grouping                  = getTag query Grouping                   s
  , comment                   = getTag query Comment                    s
  , disc                      = getTag query Disc                       s
  , label                     = getTag query Label                      s
  , musicbrainzArtistId       = getTag query MUSICBRAINZ_ARTISTID       s
  , musicbrainzAlbumId        = getTag query MUSICBRAINZ_ALBUMID        s
  , musicbrainzAlbumartistId  = getTag query MUSICBRAINZ_ALBUMARTISTID  s
  , musicbrainzTrackId        = getTag query MUSICBRAINZ_TRACKID        s
  , musicbrainzReleasetrackId = getTag query MUSICBRAINZ_RELEASETRACKID s
  , musicbrainzWorkId         = getTag query MUSICBRAINZ_WORKID         s
  }

getTag :: SongQuery s -> Metadata -> Response (SongData s) -> TagField
getTag QueryCurrent tag response =
  case response of
    Left _ -> SingleTagField Nothing
    Right maybeSong -> case maybeSong of
      Just song -> songToTagField tag song
      Nothing -> SingleTagField Nothing
getTag QueryNext tag response =
  case response of
    Left _ -> SingleTagField Nothing
    Right songs -> case songs of
      [song] -> songToTagField tag song
      _ -> SingleTagField Nothing

{- | Extract a field from the returned 'Network.MPD.Status' data record.

Helper to extract a specific field from the 'Network.MPD.Status' data
record by providing the corresponding field label. If the input status
"@st@" is /not/ @Right a@, indicating an error, or the field label
function is not applicable, it returns @Nothing@.

==== __Example__:

@
ghci> import qualified Network.MPD as MPD
ghci> st <- MPD.withMPD MPD.status
ghci> getStatusField st MPD.stVolume
@
Just (Just 100)
-}
getStatusField :: MPD.Response MPD.Status -> (MPD.Status -> a) -> Maybe a
getStatusField (Right st) f = Just (f st)
getStatusField _ _ = Nothing

{- | Go a level deeper than `getStatusField'. For nested @Maybe a@
fields from 'Network.MPD.Status'.

==== __Example__:

@
ghci> import qualified Network.MPD as MPD
ghci> st <- MPD.withMPD MPD.status
ghci> getStatusFieldElement st MPD.stVolume
@
Just 100
-}
getStatusFieldElement :: MPD.Response MPD.Status -> (MPD.Status -> Maybe a) -> Maybe a
getStatusFieldElement status item = fromMaybe Nothing $ getStatusField status item

{- | Extract a @tag@ 'Network.MPD.Value' from 'Network.MPD.Song' using
'Network.MPD.sgGetTag', convert the output to either @Maybe String@ or
@Maybe [String]@ and wrap it in 'TagField'.

Because 'Network.MPD.sgGetTag' returns @Maybe@ ['Network.MPD.Value']
where @Value@ is an instance of @ByteString@ it also offers helper
conversion functions, so convert it to @String@ if the field only
contains a list of one value or convert all ['Network.MPD.Value'] list
items to @String@ and return the list.
-}
songToTagField :: Metadata -> Song -> TagField
songToTagField tag song = tagSingleOrList (MPD.sgGetTag tag song)
  where
    tagSingleOrList :: Maybe [MPD.Value] -> TagField
    tagSingleOrList val
      | fmap length val == Just 1 =
          SingleTagField
          $ singleValueToString
          $ listToMaybe
          $ fromMaybe [] val
      | fmap length val > Just 1 =
          MultiTagField $ multiValueToString val
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

{- | Get the current 'Network.MPD.Song' relative path with 'Network.MPD.sgFilePath'
-}
maybePathCurrentSong :: MPD.Response (Maybe Song) -> Maybe String
maybePathCurrentSong cs =
  case cs of
    Left _ -> Nothing
    Right Nothing -> Nothing
    Right (Just song) -> Just $ MPD.toString $ MPD.sgFilePath song

{- | Get the next song's relative path in the current playlist.

Using 'Network.MPD.sgFilePath' from the returned 'Network.MPD.Response' @[Song]@.
-}
maybePathNextPlaylistSong :: MPD.Response [Song] -> Maybe String
maybePathNextPlaylistSong (Left _)        = Nothing
maybePathNextPlaylistSong (Right [])      = Nothing
maybePathNextPlaylistSong (Right (_:_:_)) = Nothing
maybePathNextPlaylistSong (Right [s]) =  Just $ MPD.toString $ MPD.sgFilePath s

-- | Extracts the 'Int' value from an 'Network.MPD.Id' within
-- 'Network.MPD.Status', if present and the 'Either' value is 'Right'.
getStatusIdInt :: (MPD.Status -> Maybe MPD.Id) -> Either MPD.MPDError MPD.Status -> Maybe Int
getStatusIdInt item status =
  case m of
    Just (MPD.Id int) -> Just int
    Nothing -> Nothing
  where
    m = fromMaybe Nothing $ getStatusField status item
