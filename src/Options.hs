module Options
  ( Opts(..)
  , NextSongFlag(..)
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
      flag',
      prefs,
      progDesc,
      short,
      showHelpOnEmpty,
      value,
      execParser,
      Parser,
      ParserInfo,
      infoOption,
      hidden,
      many,
      (<|>) )

import Options.Applicative.Extra ( helperWith )

import Version ( versionStr, progName )
import Data.Kind (Type)

data Opts = Opts  -- ^ Custom data record for storing 'Options.Applicative.Parser' values
  { optPort    :: Integer  -- ^ MPD port to connect.
  , optHost    :: String   -- ^ MPD host address to connect.
  , optPass    :: String   -- ^ Plain text password to connect to MPD.
  , optNext    :: NextSongFlag -- ^ Either include in the json or print it alone.
  , optVersion :: Type -> Type  -- ^ Print program version.
  }

data NextSongFlag = IncludeNextSong
              | OnlyNextSong
              | NoNextSong

optsParser :: Parser Opts
optsParser
  = Opts
  <$> portOptParser
  <*> hostOptParser
  <*> passOptParser
  <*> nextSongOptParser
  <*> versionOptParse
  where
    nextSongOptParser = nextSongFlagCountOptParser
                        <|> nextSongOnlyOptParser

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

nextSongFlagCountOptParser :: Parser NextSongFlag
nextSongFlagCountOptParser =
  fmap (intToNextSong . length) <$> many
  $ flag' ()
  $ short 'n'
  <> long "next"
  <> help ( concat
            [ "If used once (e.g. -n), include next song information in the output.\n"
            , "if used twice (e.g. -nn) it's an alias for --next-only." ])

nextSongOnlyOptParser :: Parser NextSongFlag
nextSongOnlyOptParser
  = flag' OnlyNextSong
    ( long "next-only"
      <> help "Only print next song information." )

intToNextSong :: Int -> NextSongFlag
intToNextSong count
  | count == 1 = IncludeNextSong
  | count > 1 = OnlyNextSong
  | otherwise = NoNextSong

versionOptParse :: Parser (a -> a)
versionOptParse =
  infoOption versionStr
  $ long "version"
  <> short 'V'
  <> help "Display the version number"

optsParserInfo :: ParserInfo Opts
optsParserInfo = info (optsParser <**> helper')
  $ fullDesc
  <> progDesc "Print currently playing song information as JSON"
  <> header (progName ++ " - " ++ "Current MPD song information as JSON")

helper' :: Parser (a -> a)
helper' = helperWith
          $ long "help"
          -- <> help "Show this help text"
          <> hidden -- don't show in help messages
