module Options
  ( Opts(..)
  , customExecParser
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
      customExecParser,
      helper,
      Parser,
      ParserInfo,
      infoOption )

import Version ( versionStr )
import Data.Kind (Type)

data Opts = Opts
  { optPort :: Integer
  , optHost :: String
  , optPass :: String
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
    portOptParser
      = option auto
      $ long "port"
      <> short 'p'
      <> metavar "PORTNUM"
      <> value 6600
      <> help "Port number"

    hostOptParser
      = strOption
      $ metavar "ADDRESS"
      <> long "host"
      <> short 'H'
      <> value "localhost"
      <> help "Host address"

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

optsParserInfo :: ParserInfo Opts
optsParserInfo = info (optsParser <**> helper)
  $ fullDesc
  <> progDesc "progdesc"
  <> header "header desc"
