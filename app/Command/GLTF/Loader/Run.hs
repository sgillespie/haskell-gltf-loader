module Command.GLTF.Loader.Run (run) where

import Command.GLTF.Loader.App
import Text.GLTF.Loader

import RIO

run :: RIO App ()
run = do
  file <- asks $ view (optionsL . _optionsFile)
  logInfo $ "File: " <> fromString file

  result <- liftIO $ fromFile file
  either reportError processGltf result

reportError :: HasLogFunc logger => Errors -> RIO logger ()
reportError err
  = logError (display err)
  >> exitFailure

processGltf :: Gltf -> RIO App ()
processGltf gltf = do
  let nodes = view _nodes gltf
  
  logInfo $ "Nodes: " <> display (length nodes)
  forM_ nodes $ \node -> do
    logInfo $ "  Name: " <> maybe "Unknown node" display (view _nodeName node)
