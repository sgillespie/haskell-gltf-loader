module Text.GLTF.Loader.Gltf
  ( -- * Data constructors
    Gltf(..),
    Asset(..),
    Node(..),
    -- * Lenses
    _asset,
    _assetVersion,
    _assetCopyright,
    _assetGenerator,
    _assetMinVersion,
    _nodes,
    _nodeMeshId,
    _nodeName,
    _nodeRotation,
    _nodeScale,
    _nodeTranslation,
    _nodeWeights
  ) where

import Linear.V3 (V3(..))
import Linear.V4 (V4(..))
import RIO

data Gltf = Gltf
  { gltfAsset :: Asset,
    gltfNodes :: [Node] }
  deriving (Eq, Show)

data Asset = Asset
  { assetVersion :: Text,
    assetCopyright :: Maybe Text,
    assetGenerator :: Maybe Text,
    assetMinVersion :: Maybe Text
  } deriving (Eq, Show)

data Node = Node
  { nodeMeshId :: Maybe Int,
    nodeName :: Maybe Text,
    nodeRotation :: Maybe (V4 Float),
    nodeScale :: Maybe (V3 Float),
    nodeTranslation :: Maybe (V3 Float),
    nodeWeights :: [Float]
  } deriving (Eq, Show)

_asset :: Lens' Gltf Asset
_asset = lens gltfAsset (\gltf asset -> gltf { gltfAsset = asset })

_nodes :: Lens' Gltf [Node]
_nodes = lens gltfNodes (\gltf nodes -> gltf { gltfNodes = nodes })

_assetVersion :: Lens' Asset Text
_assetVersion = lens assetVersion (\asset version' -> asset { assetVersion = version' })

_assetCopyright :: Lens' Asset (Maybe Text)
_assetCopyright = lens
  assetCopyright
  (\asset copyright' -> asset { assetCopyright = copyright' })

_assetGenerator :: Lens' Asset (Maybe Text)
_assetGenerator = lens
  assetGenerator
  (\asset generator' -> asset { assetGenerator = generator' })

_assetMinVersion :: Lens' Asset (Maybe Text)
_assetMinVersion = lens
  assetMinVersion
  (\asset minVersion' -> asset { assetMinVersion = minVersion' })

_nodeMeshId :: Lens' Node (Maybe Int)
_nodeMeshId = lens nodeMeshId (\node meshId -> node { nodeMeshId = meshId })

_nodeName :: Lens' Node (Maybe Text)
_nodeName = lens nodeName (\node name' -> node { nodeName = name' })

_nodeRotation :: Lens' Node (Maybe (V4 Float))
_nodeRotation = lens nodeRotation (\node rotation' -> node { nodeRotation = rotation' })

_nodeScale :: Lens' Node (Maybe (V3 Float))
_nodeScale = lens nodeScale (\node scale' -> node { nodeScale = scale' })

_nodeTranslation :: Lens' Node (Maybe (V3 Float))
_nodeTranslation = lens
  nodeTranslation
  (\node translation' -> node { nodeTranslation = translation' })

_nodeWeights :: Lens' Node [Float]
_nodeWeights = lens nodeWeights (\node weights' -> node { nodeWeights = weights' })
