module Text.GLTF.Loader.Gltf
  ( Gltf(..),
    Asset(..),
    Node(..),
    _nodes,
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
  { version :: Text,
    copyright :: Maybe Text,
    generator :: Maybe Text,
    minVersion :: Maybe Text
  } deriving (Eq, Show)

data Node = Node
  { name :: Maybe Text,
    rotation :: Maybe (V4 Float),
    scale :: Maybe (V3 Float),
    translation :: Maybe (V3 Float),
    weights :: [Float]
  } deriving (Eq, Show)

_asset :: Lens' Gltf Asset
_asset = lens gltfAsset (\gltf asset -> gltf { gltfAsset = asset })

_nodes :: Lens' Gltf [Node]
_nodes = lens gltfNodes (\gltf nodes -> gltf { gltfNodes = nodes })

_assetVersion :: Lens' Asset Text
_assetVersion = lens version (\asset version' -> asset { version = version' })

_assetCopyright :: Lens' Asset (Maybe Text)
_assetCopyright = lens copyright (\asset copyright' -> asset { copyright = copyright' })

_assetGenerator :: Lens' Asset (Maybe Text)
_assetGenerator = lens generator (\asset generator' -> asset { generator = generator' })

_assetMinVersion :: Lens' Asset (Maybe Text)
_assetMinVersion = lens minVersion (\asset minVersion' -> asset { minVersion = minVersion' })

_nodeName :: Lens' Node (Maybe Text)
_nodeName = lens name (\node name' -> node { name = name' })

_nodeRotation :: Lens' Node (Maybe (V4 Float))
_nodeRotation = lens rotation (\node rotation' -> node { rotation = rotation' })

_nodeScale :: Lens' Node (Maybe (V3 Float))
_nodeScale = lens scale (\node scale' -> node { scale = scale' })

_nodeTranslation :: Lens' Node (Maybe (V3 Float))
_nodeTranslation = lens
  translation
  (\node translation' -> node { translation = translation' })

_nodeWeights :: Lens' Node [Float]
_nodeWeights = lens weights (\node weights' -> node { weights = weights' })
