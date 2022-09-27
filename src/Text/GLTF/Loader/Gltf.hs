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
    -- ** Top-level Gltf Lenses
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
  { -- | The glTF version that this asset targets.
    assetVersion :: Text,
    -- | A copyright message suitable for display to credit the content creator.
    assetCopyright :: Maybe Text,
    -- | Tool that generated this glTF model.
    assetGenerator :: Maybe Text,
    -- | The minimum glTF version that this asset targets
    assetMinVersion :: Maybe Text
  } deriving (Eq, Show)

-- | The material appearance of a primitive
data Material = Material
  { -- | Specifies the cutoff threshold when in MASK alpha mode.
    materialAlphaCutoff :: Float,
    -- | The material’s alpha rendering mode enumeration specifying the interpretation of
    --   the alpha value of the base color.
    materialAlphaMode :: MaterialAlphaMode,
    -- | Specifies whether the material is double sided.
    materialDoubleSided :: Bool,
    -- | The factors for the emissive color of the material.
    materialEmissiveFactor :: V3 Float,
    -- | The user-defined name of this object.
    materialName :: Maybe Text,
    -- | Metallic roughness Physically Based Rendering (PBR) methodology parameter values.
    materialPbrMetallicRoughness :: Maybe PbrMetallicRoughness
  } deriving (Eq, Show)

-- | A set of primitives to be rendered
data Mesh = Mesh
  { -- | The user-defined name of this object.
    meshName :: Maybe Text,
    -- | A Vector of primitives, each defining geometry to be rendered.
    meshPrimitives :: Vector MeshPrimitive,
    -- | A Vector of weights to be applied to the morph targets.
    meshWeights :: Vector Float
  } deriving (Eq, Show)

-- | A node in the node hierarchy
data Node = Node
  { -- | The index of the mesh in this node.
    nodeMeshId :: Maybe Int,
    -- | The user-defined name of this object.
    nodeName :: Maybe Text,
    -- | The node's unit quaternion rotation.
    nodeRotation :: Maybe (V4 Float),
    -- | The node's non-uniform scale
    nodeScale :: Maybe (V3 Float),
    -- | The node's translation along the x, y, and z axes.
    nodeTranslation :: Maybe (V3 Float),
    -- | The weights of the instantiated morph target.
    nodeWeights :: [Float]
  } deriving (Eq, Show)

-- | Geometry to be rendered with the given material
data MeshPrimitive = MeshPrimitive
  { -- | A Vector of vertex indices.
    meshPrimitiveIndices :: Vector Word16,
    -- | The index of the material to apply to this primitive when rendering.
    meshPrimitiveMaterial :: Maybe Int,
    -- | The topology type of primitives to render.
    meshPrimitiveMode :: MeshPrimitiveMode,
    -- | A Vector of vertex normals.
    meshPrimitiveNormals :: Vector (V3 Float),
    -- | A Vector of vertex positions.
    meshPrimitivePositions :: Vector (V3 Float),
    -- | A Vector of vertex texture coordinates
    meshPrimitiveTexCoords :: Vector (V2 Float)
  } deriving (Eq, Show)

-- | Alpha rendering mode of a material
data MaterialAlphaMode
  -- | The alpha value is used to composite the source and destination areas.
  = Blend
  -- | The rendered output is either fully opaque or fully transparent depending on the
  -- alpha value and the specified alphaCutoff value.
  | Mask
  -- |The alpha value is ignored, and the rendered output is fully opaque.
  | Opaque
  deriving (Eq, Enum, Show)

-- | A set of parameter values that are used to define the metallic-roughness material
-- model from Physically-Based Rendering (PBR) methodology.
data PbrMetallicRoughness = PbrMetallicRoughness
  { -- | The factors for the base color of the material.
    pbrBaseColorFactor :: V4 Float,
    -- | The factor for the metalness of the material.
    pbrMetallicFactor :: Float,
    -- | The factor for the roughness of the material.
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

-- | Metadata about the glTF asset
_asset :: Lens' Gltf Asset
_asset = lens gltfAsset (\gltf asset -> gltf { gltfAsset = asset })

-- | A Vector of Materials. A Material defines the appearance of a primitive.
_materials :: Lens' Gltf (Vector Material)
_materials = lens gltfMaterials (\gltf mats -> gltf { gltfMaterials = mats })

-- | A Vector of Meshes. A Mesh is a set of primitives to be rendered.
_meshes :: Lens' Gltf (Vector Mesh)
_meshes = lens gltfMeshes (\gltf meshes -> gltf { gltfMeshes = meshes })

-- | A Vector of Nodes in the hierarchy.
_nodes :: Lens' Gltf (Vector Node)
_nodes = lens gltfNodes (\gltf nodes -> gltf { gltfNodes = nodes })

-- | The glTF version that this asset targets.
_assetVersion :: Lens' Asset Text
_assetVersion = lens assetVersion (\asset version' -> asset { assetVersion = version' })

-- | A copyright message suitable for display to credit the content creator.
_assetCopyright :: Lens' Asset (Maybe Text)
_assetCopyright = lens
  assetCopyright
  (\asset copyright' -> asset { assetCopyright = copyright' })

-- | Tool that generated this glTF model.
_assetGenerator :: Lens' Asset (Maybe Text)
_assetGenerator = lens
  assetGenerator
  (\asset generator' -> asset { assetGenerator = generator' })

-- | The minimum glTF version that this asset targets
_assetMinVersion :: Lens' Asset (Maybe Text)
_assetMinVersion = lens
  assetMinVersion
  (\asset minVersion' -> asset { assetMinVersion = minVersion' })

-- | Specifies the cutoff threshold when in MASK alpha mode.
_materialAlphaCutoff :: Lens' Material Float
_materialAlphaCutoff = lens
  materialAlphaCutoff
  (\material alphaCutoff -> material { materialAlphaCutoff = alphaCutoff })

-- | The material's alpha rendering mode enumeration specifying the interpretation of
--   the alpha value of the base color.
_materialAlphaMode :: Lens' Material MaterialAlphaMode
_materialAlphaMode = lens
  materialAlphaMode
  (\material mode -> material { materialAlphaMode = mode })

-- | Specifies whether the material is double sided.
_materialDoubleSided :: Lens' Material Bool
_materialDoubleSided = lens
  materialDoubleSided
  (\material doubleSided -> material { materialDoubleSided = doubleSided })

-- | The factors for the emissive color of the material.
_materialEmissiveFactor :: Lens' Material (V3 Float)
_materialEmissiveFactor = lens
  materialEmissiveFactor
  (\material emissiveFactor -> material { materialEmissiveFactor = emissiveFactor })

-- | The user-defined name of this object.
_materialName :: Lens' Material (Maybe Text)
_materialName = lens
  materialName
  (\material name -> material { materialName = name })

-- | Metallic roughness Physically Based Rendering (PBR) methodology parameter values.
_materialPbrMetallicRoughness :: Lens' Material (Maybe PbrMetallicRoughness)
_materialPbrMetallicRoughness = lens
  materialPbrMetallicRoughness
  (\material roughness -> material { materialPbrMetallicRoughness = roughness })

-- | The user-defined name of this object.
_meshName :: Lens' Mesh (Maybe Text)
_meshName = lens meshName (\mesh name -> mesh { meshName = name })

-- | A Vector of primitives, each defining geometry to be rendered.
_meshPrimitives :: Lens' Mesh (Vector MeshPrimitive)
_meshPrimitives = lens
  meshPrimitives
  (\mesh primitives -> mesh { meshPrimitives = primitives })

-- | A Vector of weights to be applied to the morph targets.
_meshWeights :: Lens' Mesh (Vector Float)
_meshWeights = lens meshWeights (\mesh weights -> mesh { meshWeights = weights })

-- | The index of the mesh in this node.
_nodeMeshId :: Lens' Node (Maybe Int)
_nodeMeshId = lens nodeMeshId (\node meshId -> node { nodeMeshId = meshId })

-- | The user-defined name of this object.
_nodeName :: Lens' Node (Maybe Text)
_nodeName = lens nodeName (\node name' -> node { nodeName = name' })

-- | The node's unit quaternion rotation.
_nodeRotation :: Lens' Node (Maybe (V4 Float))
_nodeRotation = lens nodeRotation (\node rotation' -> node { nodeRotation = rotation' })

-- | The node's non-uniform scale
_nodeScale :: Lens' Node (Maybe (V3 Float))
_nodeScale = lens nodeScale (\node scale' -> node { nodeScale = scale' })

-- | The node's translation along the x, y, and z axes.
_nodeTranslation :: Lens' Node (Maybe (V3 Float))
_nodeTranslation = lens
  nodeTranslation
  (\node translation' -> node { nodeTranslation = translation' })

-- | The weights of the instantiated morph target.
_nodeWeights :: Lens' Node [Float]
_nodeWeights = lens nodeWeights (\node weights' -> node { nodeWeights = weights' })

-- | A Vector of vertex indices.
_meshPrimitiveIndices :: Lens' MeshPrimitive (Vector Word16)
_meshPrimitiveIndices = lens
  meshPrimitiveIndices
  (\primitive' indices -> primitive' { meshPrimitiveIndices = indices })

-- | The index of the material to apply to this primitive when rendering.
_meshPrimitiveMaterial :: Lens' MeshPrimitive (Maybe Int)
_meshPrimitiveMaterial = lens
  meshPrimitiveMaterial
  (\primitive' material -> primitive' { meshPrimitiveMaterial = material })

-- | The topology type of primitives to render.
_meshPrimitiveMode :: Lens' MeshPrimitive MeshPrimitiveMode
_meshPrimitiveMode = lens
  meshPrimitiveMode
  (\primitive' mode -> primitive' { meshPrimitiveMode = mode })

-- | A Vector of vertex normals.
_meshPrimitiveNormals :: Lens' MeshPrimitive (Vector (V3 Float))
_meshPrimitiveNormals = lens
  meshPrimitiveNormals
  (\primitive' normals -> primitive' { meshPrimitiveNormals = normals })

-- | A Vector of vertex positions.
_meshPrimitivePositions :: Lens' MeshPrimitive (Vector (V3 Float))
_meshPrimitivePositions = lens
  meshPrimitivePositions
  (\primitive' positions -> primitive' { meshPrimitivePositions = positions })

-- | A Vector of vertex texture coordinates
_meshPrimitiveTexCoords :: Lens' MeshPrimitive (Vector (V2 Float))
_meshPrimitiveTexCoords = lens
  meshPrimitiveTexCoords
  (\primitive' coords -> primitive' { meshPrimitiveTexCoords = coords })

-- | The factors for the base color of the material.
_pbrBaseColorFactor :: Lens' PbrMetallicRoughness (V4 Float)
_pbrBaseColorFactor = lens
  pbrBaseColorFactor
  (\pbr baseColor -> pbr { pbrBaseColorFactor = baseColor })

-- | The factor for the metalness of the material.
_pbrMetallicFactor :: Lens' PbrMetallicRoughness Float
_pbrMetallicFactor = lens
  pbrMetallicFactor
  (\pbr metallicFactor -> pbr { pbrMetallicFactor = metallicFactor })

-- | The factor for the roughness of the material.
_pbrRoughnessFactor :: Lens' PbrMetallicRoughness Float
_pbrRoughnessFactor = lens
  pbrRoughnessFactor
  (\pbr roughnessFactor -> pbr { pbrRoughnessFactor = roughnessFactor })
