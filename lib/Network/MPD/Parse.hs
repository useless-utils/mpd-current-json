{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}


module Network.MPD.Parse where


import           Data.Aeson
  ( object, Key, Value, KeyValue((.=)), ToJSON(toJSON) )
import qualified Data.Aeson.KeyMap as KM -- Add this import
import           Data.Aeson.Types
import           Data.Kind ( Type )
import           Data.Maybe ( catMaybes, fromMaybe, listToMaybe )
import           GHC.Generics
import           Network.MPD
  ( Metadata(..), Song, PlaybackState(Stopped, Playing, Paused), Response )
import qualified Network.MPD as MPD


data TagField = SingleTagField !(Maybe String)
              | MultiTagField !(Maybe [String])
  deriving (Show, Eq)

{- | Store the parsed output of 'getTag'.

Each field represents a supported MPD tag.
-}
data Tags = Tags
  { artist                    :: !TagField
  , artistSort                :: !TagField
  , album                     :: !TagField
  , albumSort                 :: !TagField
  , albumArtist               :: !TagField
  , albumArtistSort           :: !TagField
  , title                     :: !TagField
  , track                     :: !TagField
  , name                      :: !TagField
  , genre                     :: !TagField
  , date                      :: !TagField
  , originalDate              :: !TagField
  , composer                  :: !TagField
  , performer                 :: !TagField
  , conductor                 :: !TagField
  , work                      :: !TagField
  , grouping                  :: !TagField
  , comment                   :: !TagField
  , disc                      :: !TagField
  , label                     :: !TagField
  , musicbrainzArtistId       :: !TagField
  , musicbrainzAlbumId        :: !TagField
  , musicbrainzAlbumartistId  :: !TagField
  , musicbrainzTrackId        :: !TagField
  , musicbrainzReleasetrackId :: !TagField
  , musicbrainzWorkId         :: !TagField
  }
  deriving (Show, Eq, Generic)

data Status = Status
  { psState          :: !(Maybe String)
  , psRepeat         :: !Bool
  , psRandom         :: !Bool
  , psSingle         :: !Bool
  , psConsume        :: !Bool
  , psDuration       :: !(Maybe MPD.FractionalSeconds)  -- Double
  , psElapsed        :: !(Maybe MPD.FractionalSeconds)
  , psElapsedPercent :: !(Maybe Double)
  , psVolume         :: !(Maybe Int)
  , psAudioFormat    :: !(Maybe (Int, Int, Int))
  , psBitrate        :: !(Maybe Int)
  , psCrossfade      :: !(Maybe Int)
  , psMixRampDb      :: !(Maybe Double)
  , psMixRampDelay   :: !(Maybe Double)
  , psUpdatingDb     :: !(Maybe Bool)
  , psError          :: !(Maybe String)
  }
  deriving (Show, Eq, Generic)

data PlaylistInfo = PlaylistInfo
  { piPosition     :: !(Maybe MPD.Position)
  , piNextPosition :: !(Maybe MPD.Position)
  , piId           :: !(Maybe Int)
  , piNextId       :: !(Maybe Int)
  , piLength       :: !(Maybe Int)
  }
  deriving (Show, Eq, Generic)

-- | File Information
data FileInfo = FileInfo
  { fiCurrentFile :: !(Maybe String)     -- current song file path
  , fiNextFile    :: !(Maybe String)     -- next song file path
  }
  deriving (Show, Eq, Generic)

-- | Complete MPD State
data MPDState = MPDState
  { mpdFiles    :: !FileInfo
  , mpdStatus   :: !Status
  , mpdPlaylist :: !PlaylistInfo
  , mpdTags     :: !Tags              -- Your existing Tags type
  , mpdNextTags :: !(Maybe Tags)      -- Optional next song tags
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
          , musicbrainzArtistId
          , musicbrainzAlbumId
          , musicbrainzAlbumartistId
          , musicbrainzTrackId
          , musicbrainzReleasetrackId
          , musicbrainzWorkId
          })
    = objectMaybes
      [ "artist"                     .=?? artist
      , "artist_sort"                .=?? artistSort
      , "album"                      .=?? album
      , "album_sort"                 .=?? albumSort
      , "album_artist"               .=?? albumArtist
      , "album_artist_sort"          .=?? albumArtistSort
      , "title"                      .=?? title
      , "track"                      .=?? track
      , "name"                       .=?? name
      , "genre"                      .=?? genre
      , "date"                       .=?? date
      , "original_date"              .=?? originalDate
      , "composer"                   .=?? composer
      , "performer"                  .=?? performer
      , "conductor"                  .=?? conductor
      , "work"                       .=?? work
      , "grouping"                   .=?? grouping
      , "comment"                    .=?? comment
      , "disc"                       .=?? disc
      , "label"                      .=?? label
      , "musicbrainz_artistid"       .=?? musicbrainzArtistId
      , "musicbrainz_albumid"        .=?? musicbrainzAlbumId
      , "musicbrainz_albumartistid"  .=?? musicbrainzAlbumartistId
      , "musicbrainz_trackid"        .=?? musicbrainzTrackId
      , "musicbrainz_releasetrackid" .=?? musicbrainzReleasetrackId
      , "musicbrainz_workid"         .=?? musicbrainzWorkId
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

instance ToJSON Status where
  toJSON ps = objectMaybes
    [ "state" .=? Just ps.psState
    , "repeat" .=? Just ps.psRepeat
    , "random" .=? Just ps.psRandom
    , "single" .=? Just ps.psSingle
    , "consume" .=? Just ps.psConsume
    , "duration" .=? ps.psDuration
    , "elapsed" .=? ps.psElapsed
    , "elapsed_percent" .=? ps.psElapsedPercent
    , "volume" .=? ps.psVolume
    , "audio_format" .=? ps.psAudioFormat
    , "bitrate" .=? ps.psBitrate
    , "crossfade" .=? ps.psCrossfade
    , "mixramp_db" .=? ps.psMixRampDb
    , "mixramp_delay" .=? ps.psMixRampDelay
    , "updating_db" .=? ps.psUpdatingDb
    , "error" .=? ps.psError
    ]

instance ToJSON PlaylistInfo where
  toJSON pi = objectMaybes
    [ "position" .=? pi.piPosition
    , "next_position" .=? pi.piNextPosition
    , "id" .=? pi.piId
    , "next_id" .=? pi.piNextId
    , "length" .=? pi.piLength
    ]

instance ToJSON FileInfo where
  toJSON fi = objectMaybes
    [ "filename" .=? fi.fiCurrentFile
    , "next_filename" .=? fi.fiNextFile
    ]

instance ToJSON MPDState where
  toJSON state = object $ concat
    [ objectPairs (toJSON state.mpdFiles)
    , [ "status" .= toJSON state.mpdStatus
      , "playlist" .= toJSON state.mpdPlaylist
      , "tags" .= state.mpdTags
      ]
    , case state.mpdNextTags of
        Nothing -> []
        Just nextTags -> ["next" .= object ["tags" .= nextTags]]
    ]
    where
      objectPairs (Object obj) = [(k, v) | (k, v) <- KM.toList obj]
      objectPairs _ = []

getAllTags :: SongQuery s -> Response (SongData s) -> Tags
getAllTags query s            = Tags
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
