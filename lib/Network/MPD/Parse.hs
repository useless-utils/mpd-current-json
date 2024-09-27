module Network.MPD.Parse
       -- ( getStatusField
       -- , getStatusFieldElement
       -- , getAllTags
       -- , maybePathCurrentSong
       -- , maybePathNextPlaylistSong
       -- , (.=?)
       -- , objectMaybes
       -- , getStatusIdInt
       -- , tagFieldToMaybeString )
where

import qualified Network.MPD as MPD
import Network.MPD
       ( Metadata(..), Song, PlaybackState(Stopped, Playing, Paused) )
import Data.Aeson ( object, Key, KeyValue(..), ToJSON, Value )
import Data.Aeson.Types ( Pair )
import Data.Maybe ( catMaybes, fromMaybe )

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

data SongCurrentOrNext = Current !(MPD.Response (Maybe Song))
                       | Next !(MPD.Response [Song])

getTag :: Metadata -> SongCurrentOrNext -> TagField
getTag tag (Current song) =
  case song of
    Left _ -> TagField Nothing
    Right (Just s) -> songToTagField tag s
getTag tag (Next song) =
  case song of
    Right [s] -> songToTagField tag s
    Left _    -> TagField Nothing
    _any      -> TagField Nothing

songToTagField t s = TagField $ valueToString =<< headMay =<< MPD.sgGetTag t s
-- TagField $ valueToString =<< headMay =<< MPD.sgGetTag tag s

getTagNextSong :: Metadata -> Either a [Song] -> TagField
getTagNextSong tag song =
  case song of
    Right [s] -> TagField $ MPD.sgGetTag tag s >>= headMay >>= valueToString
    Left _    -> TagField Nothing
    _any      -> TagField Nothing

{- | Use 'Network.MPD.sgGetTag' to extract a @tag@ from a @song@, safely
get only the head item of the returned @Maybe@ list, then safely
convert it to a string.
-}
processSong :: Metadata -> Maybe Song -> Maybe String
processSong _ Nothing = Nothing
processSong tag (Just song) =
  valueToString =<< headMay =<< MPD.sgGetTag tag song

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

{- | Safely get the head of a list. Same as [Safe.headMay](Safe#headMay).
-}
headMay :: [a] -> Maybe a
headMay []    = Nothing
headMay (x:_) = Just x

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
valueToString :: MPD.Value -> Maybe String
valueToString x = Just (MPD.toString x)

{- | Check if @Maybe v@ exists and is of type expected by
'Data.Aeson.object' as defined in 'Data.Aeson.Value', if it is return
both the @key@ and @value@ within the @Maybe@ context tied with
'Data.Aeson..='. This gives support to \'optional\' fields using
'Data.Maybe.catMaybes' that discard @Nothing@ values and is meant to
prevent creating JSON key/value pairs with @null@ values, e.g.:

@
jsonTags = object . catMaybes $
    [ "artist"  .=? artist
    , "album"   .=? album
    , "title"   .=? title
    ]
@

Where if a value on the right is @Nothing@ that key/value pair will
not be included in 'Data.Aeson.object' because of
'Data.Maybe.catMaybes'.
-}
(.=?) :: (KeyValue e a, ToJSON v) => Key -> Maybe v -> Maybe a
key .=? Just value = Just (key .= value)
_   .=? Nothing    = Nothing
infixr 8 .=?

-- | Helper function for creating an JSON 'Data.Aeson.object' where
-- 'Data.Maybe.catMaybes' won't include items from the '[Maybe Pair]'
-- list that return 'Nothing'.
objectMaybes :: [Maybe Pair] -> Value
objectMaybes = object . catMaybes

-- | Extracts the 'Int' value from an 'MPD.Id' within 'MPD.Status', if
-- present and the 'Either' value is a 'Right'.
getStatusIdInt :: (MPD.Status -> Maybe MPD.Id) -> Either MPD.MPDError MPD.Status -> Maybe Int
getStatusIdInt item status =
  case m of
    Just (MPD.Id int) -> Just int
    Nothing -> Nothing
  where
    m = fromMaybe Nothing $ getStatusField status item





-- #TODO new heading

newtype TagField = TagField (Maybe String)

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

tagFieldToMaybeString :: TagField -> Maybe String
tagFieldToMaybeString (TagField ms) = ms

maybeStringToTagField :: Maybe String -> TagField
maybeStringToTagField (Just ms) = TagField (Just ms)
maybeStringToTagField Nothing = TagField Nothing

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
