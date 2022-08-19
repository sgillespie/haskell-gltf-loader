module Command.GLTF.Loader.Run (run) where

import Command.GLTF.Loader.App
import Text.GLTF.Loader

import Lens.Micro
import Linear (V3(..))
import RIO
import RIO.List (nub)

run :: RIO App ()
run = do
  file <- asks $ view (optionsL . _optionsFile)
  logInfo $ "File: " <> fromString file

  result <- liftIO $ fromFile file
  either reportError reportGltf result

reportError :: HasLogFunc logger => Errors -> RIO logger ()
reportError err
  = logError (display err)
  >> exitFailure

reportGltf :: Gltf -> RIO App ()
reportGltf gltf = do
  reportAsset $ gltf ^. _asset
  
  logInfo "" -- Blank line

  reportNodes gltf

reportAsset :: Asset -> RIO App ()
reportAsset asset = do
  logInfo $ "Generator: " <> asset ^. _assetGenerator . to (fromMaybe "Unknown") . to display
  logInfo $ "Version: " <> asset ^. _assetVersion . to display

reportNodes :: Gltf -> RIO App ()
reportNodes gltf = do
  let nodes = gltf ^. _nodes
  logInfo $ "Nodes: " <> display (RIO.length nodes)

  forM_ nodes $ \node -> do
    logInfo $ "  Name: " <> maybe "Unknown node" display (view _nodeName node)

    forM_ (node ^. _nodeMeshId) $ \meshId -> do
      logInfo "  Mesh: "
      forM_ (gltf ^. _meshes  ^? ix meshId) reportMesh
        

reportMesh :: Mesh -> RIO App ()
reportMesh mesh = do
  logInfo $ "    Name: " <> mesh ^. _meshName . to (display . fromMaybe "Unknown")
  logInfo "    Mesh Primitives:"

  forM_ (mesh ^. _meshPrimitives) $ \primitive' -> do
    logInfo "      Vertex Positions:"

    forM_ (nub $ primitive' ^. _meshPrimitivePositions) $ \position -> do
      logInfo $ "        " <> displayV3 position
  
displayV3 :: Display a => V3 a -> Utf8Builder
displayV3 (V3 x y z)
  = "("
  <> display x <> ", "
  <> display y <> ", "
  <> display z <>
  ")"
