module Text.GLTF.Loader.Gltf
  ( -- * Data constructors
    Gltf(..),
    Asset(..),
    Material(..),
    MaterialAlphaMode(..),
    Mesh(..),
    Node(..),
    MeshPrimitive(..),
    PbrMetallicRoughness(..),
    MeshPrimitiveMode(..),
    -- * Lenses
    _asset,
    _materials,
    _meshes,
    _nodes,
    -- ** Asset Lenses
    _assetVersion,
    _assetCopyright,
    _assetGenerator,
    _assetMinVersion,
    -- ** Material Lenses
    _materialAlphaCutoff,
    _materialAlphaMode,
    _materialDoubleSided,
    _materialEmissiveFactor,
    _materialName,
    _materialPbrMetallicRoughness,
    -- ** Mesh Lenses
    _meshPrimitives,
    _meshWeights,
    _meshName,
    -- ** Node Lenses
    _nodeMeshId,
    _nodeName,
    _nodeRotation,
    _nodeScale,
    _nodeTranslation,
    _nodeWeights,
    -- ** MeshPrimitive Lenses
    _meshPrimitiveMaterial,
    _meshPrimitiveIndices,
    _meshPrimitiveMode,
    _meshPrimitiveNormals,
    _meshPrimitivePositions,
    -- ** PbrMetallicRoughness Lenses
    _pbrBaseColorFactor,
    _pbrMetallicFactor,
    _pbrRoughnessFactor
  ) where

import Linear
import RIO

-- | The root data type for a glTF asset
data Gltf = Gltf
  { gltfAsset :: Asset,
    gltfMaterials :: Vector Material,
    gltfMeshes :: Vector Mesh,
    gltfNodes :: Vector Node
  } deriving (Eq, Show)

-- | Metadata about the glTF asset
data Asset = Asset
  { assetVersion :: Text,
    assetCopyright :: Maybe Text,
    assetGenerator :: Maybe Text,
    assetMinVersion :: Maybe Text
  } deriving (Eq, Show)

-- | The material appearance of a primitive
data Material = Material
  { materialAlphaCutoff :: Float,
    materialAlphaMode :: MaterialAlphaMode,
    materialDoubleSided :: Bool,
    materialEmissiveFactor :: V3 Float,
    materialName :: Maybe Text,
    materialPbrMetallicRoughness :: Maybe PbrMetallicRoughness
  } deriving (Eq, Show)

-- | A set of primitives to be rendered
data Mesh = Mesh
  { meshPrimitives :: Vector MeshPrimitive,
    meshWeights :: Vector Float,
    meshName :: Maybe Text
  } deriving (Eq, Show)

-- | A node in the node hierarchy
data Node = Node
  { nodeMeshId :: Maybe Int,
    nodeName :: Maybe Text,
    nodeRotation :: Maybe (V4 Float),
    nodeScale :: Maybe (V3 Float),
    nodeTranslation :: Maybe (V3 Float),
    nodeWeights :: [Float]
  } deriving (Eq, Show)

-- | Geometry to be rendered with the given material
data MeshPrimitive = MeshPrimitive
  { meshPrimitiveIndices :: Vector Int,
    meshPrimitiveMaterial :: Maybe Int,
    meshPrimitiveMode :: MeshPrimitiveMode,
    meshPrimitiveNormals :: Vector (V3 Float),
    meshPrimitivePositions :: Vector (V3 Float),
    meshPrimitiveTexCoords :: Vector (V2 Float)
  } deriving (Eq, Show)

-- | Alpha rendering mode of a material
data MaterialAlphaMode
  = Blend
  | Mask
  | Opaque
  deriving (Eq, Enum, Show)

-- | A set of parameter values that are used to define the metallic-roughness material
-- model from Physically-Based Rendering (PBR) methodology.
data PbrMetallicRoughness = PbrMetallicRoughness
  { pbrBaseColorFactor :: V4 Float,
    pbrMetallicFactor :: Float,
    pbrRoughnessFactor :: Float
  } deriving (Eq, Show)

-- | The topology type of primitives to render.
data MeshPrimitiveMode
  = Points
  | Lines
  | LineLoop
  | LineStrip
  | Triangles
  | TriangleStrip
  | TriangleFan
  deriving (Eq, Enum, Show)

_asset :: Lens' Gltf Asset
_asset = lens gltfAsset (\gltf asset -> gltf { gltfAsset = asset })

_materials :: Lens' Gltf (Vector Material)
_materials = lens gltfMaterials (\gltf mats -> gltf { gltfMaterials = mats })

_meshes :: Lens' Gltf (Vector Mesh)
_meshes = lens gltfMeshes (\gltf meshes -> gltf { gltfMeshes = meshes })

_nodes :: Lens' Gltf (Vector Node)
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

_materialAlphaCutoff :: Lens' Material Float
_materialAlphaCutoff = lens
  materialAlphaCutoff
  (\material alphaCutoff -> material { materialAlphaCutoff = alphaCutoff })

_materialAlphaMode :: Lens' Material MaterialAlphaMode
_materialAlphaMode = lens
  materialAlphaMode
  (\material mode -> material { materialAlphaMode = mode })

_materialDoubleSided :: Lens' Material Bool
_materialDoubleSided = lens
  materialDoubleSided
  (\material doubleSided -> material { materialDoubleSided = doubleSided })

_materialEmissiveFactor :: Lens' Material (V3 Float)
_materialEmissiveFactor = lens
  materialEmissiveFactor
  (\material emissiveFactor -> material { materialEmissiveFactor = emissiveFactor })

_materialName :: Lens' Material (Maybe Text)
_materialName = lens
  materialName
  (\material name -> material { materialName = name })

_materialPbrMetallicRoughness :: Lens' Material (Maybe PbrMetallicRoughness)
_materialPbrMetallicRoughness = lens
  materialPbrMetallicRoughness
  (\material roughness -> material { materialPbrMetallicRoughness = roughness })

_meshWeights :: Lens' Mesh (Vector Float)
_meshWeights = lens meshWeights (\mesh weights -> mesh { meshWeights = weights })

_meshName :: Lens' Mesh (Maybe Text)
_meshName = lens meshName (\mesh name -> mesh { meshName = name })

_meshPrimitives :: Lens' Mesh (Vector MeshPrimitive)
_meshPrimitives = lens
  meshPrimitives
  (\mesh primitives -> mesh { meshPrimitives = primitives })

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

_meshPrimitiveIndices :: Lens' MeshPrimitive (Vector Int)
_meshPrimitiveIndices = lens
  meshPrimitiveIndices
  (\primitive' indices -> primitive' { meshPrimitiveIndices = indices })

_meshPrimitiveMaterial :: Lens' MeshPrimitive (Maybe Int)
_meshPrimitiveMaterial = lens
  meshPrimitiveMaterial
  (\primitive' material -> primitive' { meshPrimitiveMaterial = material })

_meshPrimitiveMode :: Lens' MeshPrimitive MeshPrimitiveMode
_meshPrimitiveMode = lens
  meshPrimitiveMode
  (\primitive' mode -> primitive' { meshPrimitiveMode = mode })

_meshPrimitiveNormals :: Lens' MeshPrimitive (Vector (V3 Float))
_meshPrimitiveNormals = lens
  meshPrimitiveNormals
  (\primitive' normals -> primitive' { meshPrimitiveNormals = normals })

_meshPrimitivePositions :: Lens' MeshPrimitive (Vector (V3 Float))
_meshPrimitivePositions = lens
  meshPrimitivePositions
  (\primitive' positions -> primitive' { meshPrimitivePositions = positions })

_meshPrimitiveTexCoords :: Lens' MeshPrimitive (Vector (V2 Float))
_meshPrimitiveTexCoords = lens
  meshPrimitiveTexCoords
  (\primitive' coords -> primitive' { meshPrimitiveTexCoords = coords })
  
_pbrBaseColorFactor :: Lens' PbrMetallicRoughness (V4 Float)
_pbrBaseColorFactor = lens
  pbrBaseColorFactor
  (\pbr baseColor -> pbr { pbrBaseColorFactor = baseColor })
  
_pbrMetallicFactor :: Lens' PbrMetallicRoughness Float
_pbrMetallicFactor = lens
  pbrMetallicFactor
  (\pbr metallicFactor -> pbr { pbrMetallicFactor = metallicFactor })

_pbrRoughnessFactor :: Lens' PbrMetallicRoughness Float
_pbrRoughnessFactor = lens
  pbrRoughnessFactor
  (\pbr roughnessFactor -> pbr { pbrRoughnessFactor = roughnessFactor })
