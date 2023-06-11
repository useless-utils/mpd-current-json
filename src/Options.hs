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

data Opts = Opts
  { optPort    :: Integer
  , optHost    :: String
  , optPass    :: String
  , optVersion :: Type -> Type
  }

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

    -- hostOptParser :: Parser String
    hostOptParser
      = strOption
      $ metavar "ADDRESS"
      <> long "host"
      <> short 'h'
      <> value "localhost"
      <> help "Host address"

    -- passOptParser :: Parser String
    passOptParser
      = option auto
      $ metavar "PASSWORD"
      <> long "password"
      <> short 'P'
      <> value ""
      <> help "Password for connecting (will be sent as plain text)"

    -- versionOptParse :: Parser (a -> a)
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

helper' = helperWith
          $ long "help"
          -- <> help "Show this help text"
          <> hidden -- don't show in help messages
