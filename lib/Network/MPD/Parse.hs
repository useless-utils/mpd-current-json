{-# LANGUAGE OverloadedStrings #-}
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
import Data.Maybe ( fromMaybe )
import Data.List ( (!?) )

-- | Wrapper for the output of 'getTag', which internally uses
-- 'Network.MPD.sgGetTag' to retrieve @Maybe@
-- ['Network.MPD.Value']. The list handles multi-value tags like
-- multiple artists.
data TagField = SingleTagField !(Maybe String)
              | MultiTagField !(Maybe [String])
  deriving (Show, Eq)

{- | Store the parsed output of 'getTag'.
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

{- | Extract a field from the returned MPD.Status data record.

Helper to extract a specific field from the
[Network.MPD.Status](Network.MPD#Status) data record by providing the
corresponding field label. If the input status "@st@" is /not/ @Right a@,
indicating an error, or the field label function is not applicable, it
returns @Nothing@.

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

type CurrentSong = MPD.Response (Maybe Song)
type NextSong = MPD.Response [Song]

data SongCurrentOrNext = Current !CurrentSong
                       | Next !NextSong

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

songToTagField :: Metadata -> Song -> TagField
songToTagField t s = outtag
  where
    outtag = tagSingleOrList (MPD.sgGetTag t s)
    tagSingleOrList :: Maybe [MPD.Value] -> TagField
    tagSingleOrList val | fmap length val == Just 1 = SingleTagField $ singleValueToString $ (fromMaybe [] val) !? 0
                        | fmap length val > Just 1 = MultiTagField $ multiValueToString val
                        | otherwise = SingleTagField Nothing

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

{- | Convert 'Network.MPD.Value' to @String@ within a @Maybe@ context.

This @Value@ is from 'Network.MPD' and is basically the same as a
@String@ but used internally to store metadata values.

==== __Example__:

@
processSong :: Metadata -> Maybe Song -> Maybe String
processSong _ Nothing = Nothing
processSong tag (Just song) = do
  let tagVal = MPD.sgGetTag tag song
  valueToString =<< (headMay =<< tagVal)
@

'MPD.sgGetTag' returns a @Maybe [Value]@. [libmpd](Network.MPD) also provides
'Network.MPD.toString' that can convert, along other types, a
'Network.MPD.Value' to a @String@.
-}
singleValueToString :: Maybe MPD.Value -> Maybe String
singleValueToString (Just x) = Just (MPD.toString x)
singleValueToString Nothing = Nothing

multiValueToString :: Maybe [MPD.Value] -> Maybe [String]
multiValueToString (Just x) = Just $ map MPD.toString x
multiValueToString Nothing = Nothing

-- | Extracts the 'Int' value from an 'MPD.Id' within 'MPD.Status', if
-- present and the 'Either' value is a 'Right'.
getStatusIdInt :: (MPD.Status -> Maybe MPD.Id) -> Either MPD.MPDError MPD.Status -> Maybe Int
getStatusIdInt item status =
  case m of
    Just (MPD.Id int) -> Just int
    Nothing -> Nothing
  where
    m = fromMaybe Nothing $ getStatusField status item
