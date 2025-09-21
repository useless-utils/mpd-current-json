{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Network.MPD.Parse where


import Data.Aeson
    ( object, Key, Value, KeyValue((.=)), ToJSON(toJSON) )
import Data.Aeson.Types ( Pair )
import Data.Kind ( Type )
import Data.Maybe ( catMaybes, fromMaybe, listToMaybe )
import GHC.Generics ( Generic )
import Network.MPD
       ( Metadata(..), Song, PlaybackState(Stopped, Playing, Paused), Response )
import Network.MPD qualified as MPD

data TagField = SingleTagField !(Maybe String)
              | MultiTagField !(Maybe [String])
  deriving (Show, Eq)

{- | Store the parsed output of 'getTag'.

Each field represents a supported MPD tag.
-}
data Tags = Tags
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
  deriving (Show, Eq, Generic)

-- | Type class for optional JSON serialization
class OptionalToJSON a where
  tagFieldToJSON :: a -> Maybe Value

instance OptionalToJSON TagField where
  tagFieldToJSON (SingleTagField ms) = toJSON <$> ms
  tagFieldToJSON (MultiTagField ml) = toJSON <$> ml

-- | Enhanced operator that works like .= but omits Nothing values
(.=??) :: OptionalToJSON a => Key -> a -> Maybe Pair
key .=?? field = (key .=) <$> tagFieldToJSON field
infixr 8 .=??


instance ToJSON Tags where
  toJSON (Tags
          { artist
          , artistSort
          , album
          , albumSort
          , albumArtist
          , albumArtistSort
          , title
          , track
          , name
          , genre
          , date
          , originalDate
          , composer
          , performer
          , conductor
          , work
          , grouping
          , comment
          , disc
          , label
          , musicbrainz_ArtistId
          , musicbrainz_AlbumId
          , musicbrainz_AlbumartistId
          , musicbrainz_TrackId
          , musicbrainz_ReleasetrackId
          , musicbrainz_WorkId
          })
    = objectMaybes
      [ "artist" .=?? artist
      , "artist_sort" .=?? artistSort
      , "album" .=?? album
      , "album_sort" .=?? albumSort
      , "album_artist" .=?? albumArtist
      , "album_artist_sort" .=?? albumArtistSort
      , "title" .=?? title
      , "track" .=?? track
      , "name" .=?? name
      , "genre" .=?? genre
      , "date" .=?? date
      , "original_date" .=?? originalDate
      , "composer" .=?? composer
      , "performer" .=?? performer
      , "conductor" .=?? conductor
      , "work" .=?? work
      , "grouping" .=?? grouping
      , "comment" .=?? comment
      , "disc" .=?? disc
      , "label" .=?? label
      , "musicbrainz_artistid" .=?? musicbrainz_ArtistId
      , "musicbrainz_albumid" .=?? musicbrainz_AlbumId
      , "musicbrainz_albumartistid" .=?? musicbrainz_AlbumartistId
      , "musicbrainz_trackid" .=?? musicbrainz_TrackId
      , "musicbrainz_releasetrackid" .=?? musicbrainz_ReleasetrackId
      , "musicbrainz_workid" .=?? musicbrainz_WorkId
      ]

data WhichSong = Current | Next
type family SongData (s :: WhichSong) :: Type where
  SongData 'Current = Maybe Song
  SongData 'Next = [Song]

data SongQuery (s :: WhichSong) where
  QueryCurrent :: SongQuery 'Current
  QueryNext :: SongQuery 'Next

type CurrentSong = Response (SongData 'Current)
type NextSong = Response (SongData 'Next)

-- Step 4: Function that uses the witness to extract the right type
extractFromResponse :: SongQuery s -> Response (SongData s) -> Either MPD.MPDError (SongData s)
extractFromResponse _ response = response


getAllTags :: SongQuery s -> Response (SongData s) -> Tags
getAllTags query s = Tags
  { artist                     = f query Artist                     s
  , artistSort                 = f query ArtistSort                 s
  , album                      = f query Album                      s
  , albumSort                  = f query AlbumSort                  s
  , albumArtist                = f query AlbumArtist                s
  , albumArtistSort            = f query AlbumArtistSort            s
  , title                      = f query Title                      s
  , track                      = f query Track                      s
  , name                       = f query Name                       s
  , genre                      = f query Genre                      s
  , date                       = f query Date                       s
  , originalDate               = f query OriginalDate               s
  , composer                   = f query Composer                   s
  , performer                  = f query Performer                  s
  , conductor                  = f query Conductor                  s
  , work                       = f query Work                       s
  , grouping                   = f query Grouping                   s
  , comment                    = f query Comment                    s
  , disc                       = f query Disc                       s
  , label                      = f query Label                      s
  , musicbrainz_ArtistId       = f query MUSICBRAINZ_ARTISTID       s
  , musicbrainz_AlbumId        = f query MUSICBRAINZ_ALBUMID        s
  , musicbrainz_AlbumartistId  = f query MUSICBRAINZ_ALBUMARTISTID  s
  , musicbrainz_TrackId        = f query MUSICBRAINZ_TRACKID        s
  , musicbrainz_ReleasetrackId = f query MUSICBRAINZ_RELEASETRACKID s
  , musicbrainz_WorkId         = f query MUSICBRAINZ_WORKID         s
  }
  where
    f = getTag

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

{- | Helper function for creating an JSON 'Data.Aeson.object' where
'Data.Maybe.catMaybes' won't include items from the @[Maybe
'Data.Aeson.Types.Pair']@ list that return 'Nothing'.

Meant for using with the '(.=?)' operator to remove JSON values from
the output that would contain @null@ otherwise.
-}
objectMaybes :: [Maybe Pair] -> Value
objectMaybes = object . catMaybes

-- | Convert constructor arguments of 'TagField', specially @String@
-- or @[String]@ under @Maybe@, into a 'Data.Aeson.Value' supported
-- for encoding. Since 'jsonSongTags' expects @Maybe Value@, extract
-- them from 'TagField'.
-- tagFieldToJSON

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
