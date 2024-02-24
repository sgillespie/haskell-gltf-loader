{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Command.GLTF.Loader
import Paths_gltf_loader

import Options.Applicative.Simple
import RIO
import RIO.Process

main :: IO ()
main = do
  (options', _) <- parseOptions
  logOptions <- logOptionsHandle stderr False
  processContext <- mkDefaultProcessContext

  runApp processContext options' logOptions run

parseOptions :: IO (Options, ())
parseOptions = simpleOptions version' header' description options empty
  where
    version' = $(simpleVersion version)
    header' = "Header for command line arguments"
    description = "A tool it inspect GlTF files"

runApp
  :: MonadUnliftIO m
  => ProcessContext
  -> Options
  -> LogOptions
  -> RIO App a
  -> m a
runApp processContext cliOptions logOptions appAction =
  withLogFunc logOptions $ \logFunc -> runRIO (mkApp logFunc) appAction
  where
    mkApp logFunc =
      App
        { appLogFunc = logFunc,
          appProcessContext = processContext,
          appOptions = cliOptions
        }

options :: Parser Options
options =
  Options
    <$> switch binaryOption
    <*> switch summaryOption
    <*> switch verboseOption
    <*> strArgument fileArg

binaryOption :: Mod FlagFields Bool
binaryOption =
  long "binary"
    <> short 'b'
    <> help "Load file as GLB (Binary glTF)"

summaryOption :: Mod FlagFields Bool
summaryOption =
  long "summary"
    <> short 's'
    <> help "Compact output?"

verboseOption :: Mod FlagFields Bool
verboseOption =
  long "verbose"
    <> short 'v'
    <> help "Verbose output?"

fileArg :: Mod ArgumentFields FilePath
fileArg =
  metavar "file"
    <> help "Name of the GlTF file"
    <> completer (bashCompleter "file")
