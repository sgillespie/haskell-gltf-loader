module Command.GLTF.Loader.Run (run) where

import Command.GLTF.Loader.App
import Text.GLTF.Loader

import Lens.Micro
import Lens.Micro.Platform ()
import Linear (V3(..), V4(..))
import RIO
import qualified RIO.Vector.Boxed as Vector

run :: RIO App ()
run = do
  options <- asks (^. optionsL)
  let file = options ^. _optionsFile
      summary = options ^. _optionsSummary
      verbose = options ^. _optionsVerbose

  logInfo $ "File: " <> fromString file
  result <- liftIO $ fromJsonFile file

  either reportError (reporter verbose summary) result

reporter :: Bool -> Bool -> (Gltf -> RIO App ())
reporter True _ = reportVerbose
reporter _ True = reportSummary
reporter _ _ = reportGltf

reportError :: HasLogFunc logger => Errors -> RIO logger ()
reportError err
  = logError (display err)
  >> exitFailure

reportVerbose :: Gltf -> RIO App ()
reportVerbose gltf = do
  reportAsset $ gltf ^. _asset

  logInfo "" -- Blank line

  reportNodes gltf $ reportMeshVerbose gltf

reportSummary :: Gltf -> RIO App ()
reportSummary gltf = do
  reportAsset $ gltf ^. _asset
  
  logInfo "" -- Blank line

  reportNodes gltf reportMeshSummary

reportGltf :: Gltf -> RIO App ()
reportGltf gltf = do
  reportAsset $ gltf ^. _asset
  
  logInfo "" -- Blank line

  reportNodes gltf $ reportMesh gltf

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
      forM_ (gltf ^. _meshes ^? ix meshId) meshReporter

reportMeshVerbose :: Gltf -> Mesh -> RIO App ()
reportMeshVerbose gltf mesh = do
  logInfo $ "    Name: " <> mesh ^. _meshName . to (display . fromMaybe "Unknown")
  logInfo "    Mesh Primitives:"

  forM_ (mesh ^. _meshPrimitives) $ \primitive' -> do
    -- Report Vertices
    logInfo "      Vertices:"
    
    let positions = primitive' ^. _meshPrimitivePositions
    forM_ (primitive' ^. _meshPrimitiveIndices) $ \index -> do
      forM_ (positions Vector.!? fromIntegral index) $ \position -> do
        logInfo $ "        [" <> display index <> "]: " <> displayV3 position

    logInfo "      Normals:"
    let normals = primitive' ^. _meshPrimitiveNormals
    forM_ (primitive' ^. _meshPrimitiveIndices) $ \index -> do
      forM_ (normals Vector.!? fromIntegral index) $ \normal -> do
        logInfo $ "        [" <> display index <> "]: " <> displayV3 normal

    -- Report material
    forM_ (primitive' ^. _meshPrimitiveMaterial) $ \materialId -> do
      forM_ (gltf ^. _materials . to (Vector.!? materialId)) reportMaterial
    
reportMesh :: Gltf -> Mesh -> RIO App ()
reportMesh gltf mesh = do
  logInfo $ "    Name: " <> mesh ^. _meshName . to (display . fromMaybe "Unknown")
  logInfo "    Mesh Primitives:"

  forM_ (mesh ^. _meshPrimitives) $ \primitive' -> do
    logInfo "      Vertex Positions:"

    forM_ (Vector.uniq $ primitive' ^. _meshPrimitivePositions) $ \position -> do
      logInfo $ "        " <> displayV3 position

    -- Report material
    forM_ (primitive' ^. _meshPrimitiveMaterial) $ \materialId -> do
      forM_ (gltf ^. _materials . to (Vector.!? materialId)) reportMaterial

reportMeshSummary :: Mesh -> RIO App ()
reportMeshSummary mesh = do
  logInfo $ "    Name: " <> mesh ^. _meshName . to (display . fromMaybe "Unknown")

  let primitives' = mesh ^. _meshPrimitives
      vertices = Vector.concatMap (^. _meshPrimitivePositions) primitives'
      indices = Vector.concatMap (^. _meshPrimitiveIndices) primitives'

  logInfo $ "    Unique Vertices: " <> display (length vertices)
  logInfo $ "    Total Vertices: " <> display (length indices)

reportMaterial :: Material -> RIO App ()
reportMaterial material = do
  logInfo "      Material:"
  logInfo $ "        Name: " <>
    material ^. _materialName . to (display . fromMaybe "Unknown")
  
  forM_ (material ^. _materialPbrMetallicRoughness) $ \pbr -> do
    logInfo $ "        Base Color Factor: " <> pbr ^. _pbrBaseColorFactor . to displayV4
    logInfo $ "        Metallic Factor: " <> pbr ^. _pbrMetallicFactor . to display
    logInfo $ "        Roughness Factor: " <> pbr ^. _pbrRoughnessFactor . to display
  
displayV3 :: Display a => V3 a -> Utf8Builder
displayV3 (V3 x y z)
  = "("
  <> display x <> ", "
  <> display y <> ", "
  <> display z <>
  ")"

displayV4 :: Display a => V4 a -> Utf8Builder
displayV4 (V4 w x y z)
  = "("
  <> display w <> ", "
  <> display x <> ", "
  <> display y <> ", "
  <> display z <>
  ")"

