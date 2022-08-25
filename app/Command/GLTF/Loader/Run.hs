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
  summary <- asks $ view (optionsL . _optionsSummary)
  logInfo $ "File: " <> fromString file

  result <- liftIO $ fromFile file

  let reporter = if summary
        then reportSummary
        else reportGltf
        
  either reportError reporter result

reportError :: HasLogFunc logger => Errors -> RIO logger ()
reportError err
  = logError (display err)
  >> exitFailure

reportSummary :: Gltf -> RIO App ()
reportSummary gltf = do
  reportAsset $ gltf ^. _asset
  
  logInfo "" -- Blank line

  reportNodes gltf reportMeshSummary

reportGltf :: Gltf -> RIO App ()
reportGltf gltf = do
  reportAsset $ gltf ^. _asset
  
  logInfo "" -- Blank line

  reportNodes gltf reportMesh

reportAsset :: Asset -> RIO App ()
reportAsset asset = do
  logInfo $ "Generator: " <> asset ^. _assetGenerator . to (fromMaybe "Unknown") . to display
  logInfo $ "Version: " <> asset ^. _assetVersion . to display

reportNodes :: Gltf -> (Mesh -> RIO App ()) -> RIO App ()
reportNodes gltf meshReporter = do
  let nodes = gltf ^. _nodes
  logInfo $ "Nodes: " <> display (RIO.length nodes)

  forM_ nodes $ \node -> do
    logInfo $ "  Name: " <> maybe "Unknown node" display (view _nodeName node)

    forM_ (node ^. _nodeMeshId) $ \meshId -> do
      logInfo "  Mesh: "
      forM_ (gltf ^. _meshes  ^? ix meshId) meshReporter
        

reportMesh :: Mesh -> RIO App ()
reportMesh mesh = do
  logInfo $ "    Name: " <> mesh ^. _meshName . to (display . fromMaybe "Unknown")
  logInfo "    Mesh Primitives:"

  forM_ (mesh ^. _meshPrimitives) $ \primitive' -> do
    logInfo "      Vertex Positions:"

    forM_ (nub $ primitive' ^. _meshPrimitivePositions) $ \position -> do
      logInfo $ "        " <> displayV3 position

reportMeshSummary :: Mesh -> RIO App ()
reportMeshSummary mesh = do
  logInfo $ "    Name: " <> mesh ^. _meshName . to (display . fromMaybe "Unknown")

  let primitives' = mesh ^. _meshPrimitives
      vertices = concatMap (^. _meshPrimitivePositions) primitives'
      indices = concatMap (^. _meshPrimitiveIndices) primitives'

  logInfo $ "    Unique Vertices: " <> display (length vertices)
  logInfo $ "    Total Vertices: " <> display (length indices)
  
displayV3 :: Display a => V3 a -> Utf8Builder
displayV3 (V3 x y z)
  = "("
  <> display x <> ", "
  <> display y <> ", "
  <> display z <>
  ")"
