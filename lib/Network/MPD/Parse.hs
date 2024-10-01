module Network.MPD.Parse
  ( TagField (..)
  , ExtractedTags (..)
  , getAllTags
  , getStatusField
  , getStatusFieldElement
  , SongCurrentOrNext(..)
  , getTag
  , songToTagField
  , maybePathCurrentSong
  , maybePathNextPlaylistSong
  , singleValueToString
  , multiValueToString
  , getStatusIdInt
  )
where

import qualified Network.MPD as MPD
import Network.MPD
       ( Metadata(..), Song, PlaybackState(Stopped, Playing, Paused) )
import Data.Maybe ( fromMaybe, listToMaybe )

{- | Wrapper for the output of 'getTag', which internally uses
'Network.MPD.sgGetTag' to retrieve @Maybe@ ['Network.MPD.Value'] that
are then converted to @TagField@. This allows handling multi-value
tags like multiple artists.
-}
data TagField = SingleTagField !(Maybe String)
              | MultiTagField !(Maybe [String])
  deriving (Show, Eq)

{- | Store the parsed output of 'getTag'.

Each field represents a supported MPD tag.
-}
data ExtractedTags = ExtractedTags
  { artist                     :: !TagField
  , artistSort                 :: !TagField
  , album                      :: !TagField
  , albumSort                  :: !TagField
  , albumArtist                :: !TagField
  , albumArtistSort            :: !TagField
  , title                      :: !TagField
  , track                      :: !TagField
  , name                       :: !TagField
  , genre                      :: !TagField
  , date                       :: !TagField
  , originalDate               :: !TagField
  , composer                   :: !TagField
  , performer                  :: !TagField
  , conductor                  :: !TagField
  , work                       :: !TagField
  , grouping                   :: !TagField
  , comment                    :: !TagField
  , disc                       :: !TagField
  , label                      :: !TagField
  , musicbrainz_ArtistId       :: !TagField
  , musicbrainz_AlbumId        :: !TagField
  , musicbrainz_AlbumartistId  :: !TagField
  , musicbrainz_TrackId        :: !TagField
  , musicbrainz_ReleasetrackId :: !TagField
  , musicbrainz_WorkId         :: !TagField
  }

{- | Assign 'getTag' returned values to 'ExtractedTags'.

Takes either a song @Current s@ or @Next s@, because their object
format differs, see 'SongCurrentOrNext'.
-}
getAllTags :: SongCurrentOrNext -> ExtractedTags
getAllTags s = ExtractedTags
  { artist                     = f Artist                     s
  , artistSort                 = f ArtistSort                 s
  , album                      = f Album                      s
  , albumSort                  = f AlbumSort                  s
  , albumArtist                = f AlbumArtist                s
  , albumArtistSort            = f AlbumArtistSort            s
  , title                      = f Title                      s
  , track                      = f Track                      s
  , name                       = f Name                       s
  , genre                      = f Genre                      s
  , date                       = f Date                       s
  , originalDate               = f OriginalDate               s
  , composer                   = f Composer                   s
  , performer                  = f Performer                  s
  , conductor                  = f Conductor                  s
  , work                       = f Work                       s
  , grouping                   = f Grouping                   s
  , comment                    = f Comment                    s
  , disc                       = f Disc                       s
  , label                      = f Label                      s
  , musicbrainz_ArtistId       = f MUSICBRAINZ_ARTISTID       s
  , musicbrainz_AlbumId        = f MUSICBRAINZ_ALBUMID        s
  , musicbrainz_AlbumartistId  = f MUSICBRAINZ_ALBUMARTISTID  s
  , musicbrainz_TrackId        = f MUSICBRAINZ_TRACKID        s
  , musicbrainz_ReleasetrackId = f MUSICBRAINZ_RELEASETRACKID s
  , musicbrainz_WorkId         = f MUSICBRAINZ_WORKID         s
  }
  where
    f = getTag

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

-- | Alias for the output of 'Network.MPD.currentSong'.
type CurrentSong = MPD.Response (Maybe Song)

-- | Alias for the output of 'Network.MPD.playlistInfo'.
type NextSong = MPD.Response [Song]

-- | Wrapper for 'getTag' to expect either @Maybe Song@ or
-- @[Song]@. This simplifies 'getAllTags'.
data SongCurrentOrNext = Current !CurrentSong
                       | Next !NextSong

-- | Retrieve @tag@, which should be one of 'Network.MPD.Metadata', from
-- 'CurrentSong' or 'NextSong'.
getTag :: Metadata -> SongCurrentOrNext -> TagField
getTag tag (Current song) =
  case song of
    Left _ -> SingleTagField Nothing
    Right (Just s) -> songToTagField tag s
getTag tag (Next song) =
  case song of
    Right [s] -> songToTagField tag s
    Left _    -> SingleTagField Nothing
    _any      -> SingleTagField Nothing

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
