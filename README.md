- [Installation](#org612f7f8)
- [Usage](#org31b2d26)
- [Files](#org839022f)
- [Changelog](#orga0126ff)



<a id="org612f7f8"></a>

# Installation

```
git clone https://codeberg.org/useless-utils/mpd-current-json
cd mpd-current-json
```

and to install the executable to `./dist`, in the current directory:

    cabal install --install-method=copy --overwrite-policy=always --installdir=dist

or to install to `${CABAL_DIR}/bin` remove the `--installdir=dist` argument. `CABAL_DIR` defaults to `~/.local/state/cabal`.


<a id="org31b2d26"></a>

# Usage

get values

    mpd-current-json | jaq .tags.album
    mpd-current-json | jaq .status.elapsed_percent

provide host and port with

    mpd-current-json -h 'localhost' -p 4321


<a id="org839022f"></a>

# Files


## Source


### Main.hs

1.  Pragma language extensions

    ```haskell
    {-# LANGUAGE OverloadedStrings #-}
    ```

2.  Imports

    Import for the `libmpd` library, added as `libmpd == 0.10.*` to [mpd-current-json.cabal](#org997dd38).
    
    ```haskell
    import qualified Network.MPD as MPD
    import Network.MPD
        ( Metadata(..), Song, PlaybackState(Stopped, Playing, Paused) )
    import Data.Maybe ( catMaybes )
    import Data.Aeson ( object, Key, KeyValue(..), ToJSON )
    import Data.Aeson.Encode.Pretty ( encodePretty )
    import qualified Data.ByteString.Lazy.Char8 as C
    import Text.Printf ( printf )
    import Options
    ```

3.  Main

    ```haskell
    main :: IO ()
    main = do
    ```
    
    Parse the command-line options and bind the result to `opts`.
    
    ```haskell
      opts <- execParser optsParserInfo
    ```
    
    Connect to MPD using either the provided arguments from the command-line or the default values, as defined in [​`Parser Opts` definition](#org8d52a74).
    
    ```haskell
      cs <- MPD.withMPDEx (optHost opts) (optPort opts) (optPass opts) MPD.currentSong
      st <- MPD.withMPDEx (optHost opts) (optPort opts) (optPass opts) MPD.status
    ```
    
    where `currentSong` returns a `Maybe (Just (Song {...}))` and `status` returns `Maybe (Status {...})` to be parsed.
    
    The data record `Song` from the command `currentSong` contains a field label "`sgTags`" that contains all embedded metadata tags in a `fromList [...]`, in this `let` statement store the parser `getTag` function calls to be placed in the JSON object later:
    
    ```haskell
      let artist                     = getTag Artist                     cs
          artistSort                 = getTag ArtistSort                 cs
          album                      = getTag Album                      cs
          albumSort                  = getTag AlbumSort                  cs
          albumArtist                = getTag AlbumArtist                cs
          albumArtistSort            = getTag AlbumArtistSort            cs
          title                      = getTag Title                      cs
          track                      = getTag Track                      cs
          name                       = getTag Name                       cs
          genre                      = getTag Genre                      cs
          date                       = getTag Date                       cs
          originalDate               = getTag OriginalDate               cs
          composer                   = getTag Composer                   cs
          performer                  = getTag Performer                  cs
          conductor                  = getTag Conductor                  cs
          work                       = getTag Work                       cs
          grouping                   = getTag Grouping                   cs
          comment                    = getTag Comment                    cs
          disc                       = getTag Disc                       cs
          label                      = getTag Label                      cs
          musicbrainz_Artistid       = getTag MUSICBRAINZ_ARTISTID       cs
          musicbrainz_Albumid        = getTag MUSICBRAINZ_ALBUMID        cs
          musicbrainz_Albumartistid  = getTag MUSICBRAINZ_ALBUMARTISTID  cs
          musicbrainz_Trackid        = getTag MUSICBRAINZ_TRACKID        cs
          musicbrainz_Releasetrackid = getTag MUSICBRAINZ_RELEASETRACKID cs
          musicbrainz_Workid         = getTag MUSICBRAINZ_WORKID         cs
    ```
    
    Likewise, `getStatusItem` parses values from `Status {...}` returned by `status`, some may require additional `Maybe` checks to get the desired values.
    
    ```haskell
      let state :: Maybe String
          state = case getStatusItem st MPD.stState of
                    Just ps -> case ps of
                                 Playing -> Just "play"  -- same as mpc
                                 Paused  -> Just "pause"  -- same as mpc
                                 Stopped -> Just "stopped"
                    Nothing -> Nothing
    
          time = getStatusItem st MPD.stTime
    
          elapsed = case time of
            Just t -> case t of
                        Just (e, _) -> Just e
                        _ -> Nothing
            Nothing -> Nothing
    
          duration = case time of
            Just t -> case t of
                        Just (_, d) -> Just d
                        _ -> Nothing
            Nothing -> Nothing
    
          elapsedPercent :: Maybe Double
          elapsedPercent = case time of
            Just t -> case t of
                        Just t1 -> Just (read $ printf "%.2f" (uncurry (/) t1 * 100))
                        Nothing -> Just 0
            Nothing -> Nothing
    
          repeatSt = getStatusItem st MPD.stRepeat
          randomSt = getStatusItem st MPD.stRandom
          singleSt = getStatusItem st MPD.stSingle
          consumeSt = getStatusItem st MPD.stConsume
          pos = getStatusItem st MPD.stSongPos
          playlistLength = getStatusItem st MPD.stPlaylistLength
          bitrate = getStatusItem st MPD.stBitrate
          audioFormat = getStatusItem st MPD.stAudio
          errorSt = getStatusItem st MPD.stError
    ```
    
    The `object . catMaybes` constructs a JSON object by combining a list of key/value pairs. The `.=?` operator is used to create each key/value pair. If the value is `Just`, the key/value pair is included in the list; if the value is `Nothing`, it is filtered out using `catMaybes` to prevent generating fields with a value of `null` in the final JSON object. Then, the `object` function converts the list of key/value pairs into an `Object` data structure that can be 'encoded' using `Data.Aeson`'s "`encode`" or `Data.Aeson.Encode.Pretty`'s "`encodePretty`".
    
    ```haskell
      -- sgTags
      let jTags = object . catMaybes $
            [ "artist"                     .=? artist
            , "artist_sort"                .=? artistSort
            , "album"                      .=? album
            , "album_sort"                 .=? albumSort
            , "album_artist"               .=? albumArtist
            , "album_artist_sort"          .=? albumArtistSort
            , "title"                      .=? title
            , "track"                      .=? track
            , "name"                       .=? name
            , "genre"                      .=? genre
            , "date"                       .=? date
            , "original_date"              .=? originalDate
            , "composer"                   .=? composer
            , "performer"                  .=? performer
            , "conductor"                  .=? conductor
            , "work"                       .=? work
            , "grouping"                   .=? grouping
            , "comment"                    .=? comment
            , "disc"                       .=? disc
            , "label"                      .=? label
            , "musicbrainz_artistid"       .=? musicbrainz_Artistid
            , "musicbrainz_albumid"        .=? musicbrainz_Albumid
            , "musicbrainz_albumartistid"  .=? musicbrainz_Albumartistid
            , "musicbrainz_trackid"        .=? musicbrainz_Trackid
            , "musicbrainz_releasetrackid" .=? musicbrainz_Releasetrackid
            , "musicbrainz_workid"         .=? musicbrainz_Workid
            ]
    
      -- status
      let jStatus = object . catMaybes $
            [ "state"           .=? state
            , "repeat"          .=? repeatSt
            , "elapsed"         .=? elapsed
            , "duration"        .=? duration
            , "elapsed_percent" .=? elapsedPercent
            , "random"          .=? randomSt
            , "single"          .=? singleSt
            , "consume"         .=? consumeSt
            , "song_position"   .=? pos
            , "playlist_length" .=? playlistLength
            , "bitrate"         .=? bitrate
            , "audio_format"    .=? audioFormat
            , "error"           .=? errorSt
            ]
    ```
    
    Having two objects, one for "tags" and other for "status", create a nested JSON with labels before each of them.
    
    ```haskell
      let jObject = object [ "tags" .= jTags
                           , "status" .= jStatus ]
    ```
    
    e.g. so they can be parsed as "`.tags.title`" or "`.status.elapsed_percent`".
    
    Finally, encode it to real JSON and print it to the terminal. `Data.Aeson`'s encoding is returned as a `ByteString` so use the `Data.ByteString...` import that provides an implementation of `putStrLn` that supports `ByteString`​s.
    
    ```haskell
      C.putStrLn $ encodePretty jObject
    ```
    
    1.  Utility Functions
    
        The `getStatusItem` function takes an `Either MPD.MPDError MPD.Status` value and a field label function `f` as arguments. It returns `Just (f st)` if the input status is `Right st`, where `st` is the `MPD.Status` value. This function helps to extract a specific field from the status by providing the corresponding field label function. If the input status is not `Right st`, indicating an error, or the field label function is not applicable, it returns `Nothing`.
        
        ```haskell
        getStatusItem :: Either MPD.MPDError MPD.Status -> (MPD.Status -> a) -> Maybe a
        getStatusItem (Right st) f = Just (f st)
        getStatusItem _ _ = Nothing
        ```
        
        The `getTag` function takes a metadata type `t` and an `Either` value `c` containing a `Maybe Song`. It checks if the `Either` value is `Left _`, indicating an error, and returns `Nothing`. If the `Either` value is `Right song`, it calls the `processSong` function with the metadata type `t` and the `Just song` value, which extracts the tag value from the song. The `getTag` function helps to retrieve a specific tag value from the song if it exists.
        
        ```haskell
        getTag :: Metadata -> Either a (Maybe Song) -> Maybe String
        getTag t c =
          case c of
            Left _ -> Nothing
            Right song -> processSong t song
        ```
        
        The `processSong` function takes a metadata type `tag` and a `Maybe Song`. If the `Maybe Song` value is `Nothing`, indicating an empty value, it returns `Nothing`. If the `Maybe Song` value is `Just song`, it retrieves the tag value using the `MPD.sgGetTag` function with the provided metadata type and song. It then applies the `headMay` function to extract the first element from the list of tag values and the `valueToStringMay` function to convert the value to a string within a `Maybe` context. This function helps to process the tag values of a song and convert them to strings if they exist.
        
        ```haskell
        processSong :: Metadata -> Maybe Song -> Maybe String
        processSong _ Nothing = Nothing
        processSong tag (Just song) = do
          let tagVal = MPD.sgGetTag tag song
          valueToStringMay =<< (headMay =<< tagVal)
        ```
        
        The `headMay` function is a utility function that safely gets the head of a list. It takes a list as input and returns `Nothing` if the list is empty or `Just x` where `x` is the first element of the list.
        
        ```haskell
        -- Utility function to safely get the head of a list
        headMay :: [a] -> Maybe a
        headMay []    = Nothing
        headMay (x:_) = Just x
        ```
        
        The `valueToStringMay` function is a utility function that converts a `MPD.Value` to a `String` within a `Maybe` context. It takes a `MPD.Value` as input and returns `Just (MPD.toString x)` where `x` is the input value converted to a string.
        
        ```haskell
        -- Utility function to convert Value to String within a Maybe context
        valueToStringMay :: MPD.Value -> Maybe String
        valueToStringMay x = Just (MPD.toString x)
        ```
        
        The `.=?` operator is a utility function to define optional fields in the key-value pairs of a JSON object. It takes a `Key` and a `Maybe` value `v` as input. If the `Maybe` value is `Just value`, it returns `Just (key .= value)`, where `key` is the input key and `value` is the input value. If the `Maybe` value is `Nothing`, it returns `Nothing`. This operator helps to conditionally include or exclude fields in the JSON object based on the presence or absence of values.
        
        ```haskell
        -- Utility function to define optional fields
        (.=?) :: (KeyValue a, ToJSON v) => Key -> Maybe v -> Maybe a
        key .=? Just value = Just (key .= value)
        _   .=? Nothing    = Nothing
        ```


### Options.hs

```haskell
module Options
  ( Opts(..)
  , execParser
  , prefs
  , showHelpOnEmpty
  , optsParser
  , optsParserInfo ) where

import Options.Applicative
    ( (<**>),
      auto,
      fullDesc,
      header,
      help,
      info,
      long,
      metavar,
      option,
      strOption,
      prefs,
      progDesc,
      short,
      showHelpOnEmpty,
      value,
      execParser,
      helper,
      Parser,
      ParserInfo,
      infoOption,
      hidden )

import Options.Applicative.Extra ( helperWith )

import Version ( versionStr, progName )
import Data.Kind (Type)

```

1.  Data record for holding parsed 'Parser' values

    ```haskell
    data Opts = Opts
      { optPort    :: Integer
      , optHost    :: String
      , optPass    :: String
      , optVersion :: Type -> Type
      }
    ```

2.  `Parser Opts` definition

    > A [Parser](https://hackage.haskell.org/package/optparse-applicative-0.18.1.0/docs/Options-Applicative.html#t:Parser) a is an option parser returning a value of type a.
    
    Specify how `Options.Applicative` should parse arguments. Their returned values are stored in the custom defined data record `Opts`.
    
    ```haskell
    optsParser :: Parser Opts
    optsParser
      = Opts
      <$> portOptParser
      <*> hostOptParser
      <*> passOptParser
      <*> versionOptParse
      where
        portOptParser :: Parser Integer
        portOptParser
          = option auto
          $ long "port"
          <> short 'p'
          <> metavar "PORTNUM"
          <> value 6600
          <> help "Port number"
    
        hostOptParser :: Parser String
        hostOptParser
          = strOption
          $ metavar "ADDRESS"
          <> long "host"
          <> short 'h'
          <> value "localhost"
          <> help "Host address"
    
        passOptParser :: Parser String
        passOptParser
          = option auto
          $ metavar "PASSWORD"
          <> long "password"
          <> short 'P'
          <> value ""
          <> help "Password for connecting (will be sent as plain text)"
    
        versionOptParse :: Parser (a -> a)
        versionOptParse =
          infoOption versionStr
          $ long "version"
          <> short 'V'
          <> help "Display the version number"
    ```

3.  Create ParserInfo

    > A [ParserInfo](https://hackage.haskell.org/package/optparse-applicative-0.18.1.0/docs/Options-Applicative.html#t:ParserInfo) describes a command line program, used to generate a help screen. &#x2014; [Options.Applicative](https://hackage.haskell.org/package/optparse-applicative-0.18.1.0/docs/Options-Applicative.html#g:8)
    
    -   `optsParserInfo`
        
        Utility function for `Options.Applicative`'s "`info`" that create a `ParserInfo` given a [​`Parser`​](https://hackage.haskell.org/package/optparse-applicative-0.18.1.0/docs/Options-Applicative.html#t:Parser) and a modifier, where `Parser`​s are defined using a [​custom data record​](#orga04dbef).
    
    ```haskell
    optsParserInfo :: ParserInfo Opts
    optsParserInfo = info (optsParser <**> helper')
      $ fullDesc
      <> progDesc "Print currently playing song information as JSON"
      <> header (progName ++ " - " ++ "Current MPD song information as JSON")
    ```

4.  Custom helper

    > Like helper, but with a minimal set of modifiers that can be extended as desired.
    > 
    > ```haskell
    >   opts :: ParserInfo Sample
    >   opts = info (sample <**> helperWith (mconcat [
    >            long "help",
    >            short 'h',
    >            help "Show this help text",
    >            hidden
    >          ])) mempty
    > ```
    > 
    > &#x2014; source of [Options.Applicative#helper](https://hackage.haskell.org/package/optparse-applicative-0.18.1.0/docs/Options-Applicative.html#v:helper)
    
    Define a helper command that only accepts long `--help`:
    
    ```haskell
    helper' = helperWith
              $ long "help"
              -- <> help "Show this help text"
              <> hidden -- don't show in help messages
    ```


### Version.hs

```haskell
module Version ( versionStr,
                 progName ) where

import Data.Version (showVersion)

import Paths_mpd_current_json (version) -- generated by Cabal

progName :: [Char]
progName = "mpd-current-json"

versionStr :: [Char]
versionStr = progName ++ " version " ++ (showVersion version)
```


## Extra


### CHANGELOG.org

File to be tangled and include the [Changelog](#orga0126ff) heading.


### mpd-current-json.cabal

```haskell-cabal
cabal-version:      3.0
name:               mpd-current-json
-- The package version.
-- See the Haskell package versioning policy (PVP) for standards
-- guiding when and how versions should be incremented.
-- https://pvp.haskell.org
-- PVP summary:     +-+------- breaking API changes
--                  | | +----- non-breaking API additions
--                  | | | +--- code changes with no API change
version:            1.1.0.0
synopsis:           Print current MPD song and status as json

-- A longer description of the package.
-- description:
homepage:           https://codeberg.org/useless-utils/mpd-current-json

-- A URL where users can report bugs.
-- bug-reports:
license:            Unlicense
license-file:       UNLICENSE
author:             Lucas G
maintainer:         g@11xx.org

-- A copyright notice.
-- copyright:
category:           Network
extra-source-files: CHANGELOG.org

executable mpd-current-json
    main-is:          Main.hs

    -- Modules included in this executable, other than Main.
    other-modules:    Options
                      Paths_mpd_current_json
                      Version

    -- LANGUAGE extensions used by modules in this package.
    -- other-extensions:
    build-depends:    base ^>=4.16.4.0
                    , libmpd == 0.10.*
                    , optparse-applicative == 0.18.*
                    , aeson == 2.1.*
                    , bytestring == 0.11.*
                    , aeson-pretty == 0.8.*

    -- Directories containing source files.
    hs-source-dirs:   src
    default-language: Haskell2010

    -- [[https://kowainik.github.io/posts/2019-02-06-style-guide#ghc-options][Haskell Style Guide :: Kowainik]]
    ghc-options:    -Wall
                    -Wcompat
                    -Widentities
                    -Wincomplete-uni-patterns
                    -Wincomplete-record-updates
                    -Wredundant-constraints
                    -Wmissing-export-lists
                    -Wpartial-fields
                    -Wmissing-deriving-strategies
                    -Wunused-packages
                    -fwrite-ide-info
                    -hiedir=.hie
                    -fprof-auto -fprof-cafs

```


<a id="orga0126ff"></a>

# Changelog


## v1.1.0.0

-   Remove `-h` from `--help` and use `-h` for `--host`
-   Make `--help` option hidden in the help message


## v1.0.0.0

Initial working version

-   Added conditional tags printing, only non-empty values are printed
-   Accept host, port and password
-   Nested json objects for \`status' and \`tags'
-   Added elapsed<sub>percent</sub> key shortcut for \`elapsed / duration \* 100'


## v0.0.1.0

-   initial connection and parsing values

-   First version. Released on an unsuspecting world.