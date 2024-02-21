module Text.GLTF.Loader.Gltf
  ( -- * Data constructors
    Gltf(..),
    Asset(..),
    Image(..),
    MagFilter(..),
    MinFilter(..),
    Material(..),
    MaterialAlphaMode(..),
    Mesh(..),
    Node(..),
    MeshPrimitive(..),
    PbrMetallicRoughness(..),
    MeshPrimitiveMode(..),
    Sampler(..),
    SamplerWrap(..),
    Texture(..),
    TextureInfo(..),
    -- * Lenses
    -- ** Top-level Gltf Lenses
    _asset,
    _images,
    _materials,
    _meshes,
    _nodes,
    _samplers,
    _textures,
    -- ** Asset Lenses
    _assetVersion,
    _assetCopyright,
    _assetGenerator,
    _assetMinVersion,
    -- ** Image Lenses
    _imageData,
    _imageMimeType,
    _imageName,
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
    _nodeChildren,
    _nodeMeshId,
    _nodeName,
    _nodeRotation,
    _nodeScale,
    _nodeTranslation,
    _nodeWeights,
    -- ** Sampler Lenses
    _samplerMagFilter,
    _samplerMinFilter,
    _samplerName,
    _samplerWrapS,
    _samplerWrapT,
    -- ** Texture Lenses
    _textureName,
    _textureSamplerId,
    _textureSourceId,
    -- ** MeshPrimitive Lenses
    _meshPrimitiveMaterial,
    _meshPrimitiveIndices,
    _meshPrimitiveMode,
    _meshPrimitiveNormals,
    _meshPrimitivePositions,
    _meshPrimitiveColors,
    -- ** PbrMetallicRoughness Lenses
    _pbrBaseColorFactor,
    _pbrBaseColorTexture,
    _pbrMetallicFactor,
    _pbrRoughnessFactor,
    -- ** TextureInfo Lenses
    _textureInfoId,
    _textureInfoTexCoord
  ) where

import Linear
import RIO

-- | The root data type for a glTF asset
data Gltf = Gltf
  { gltfAsset :: Asset,
    gltfImages :: Vector Image,
    gltfMaterials :: Vector Material,
    gltfMeshes :: Vector Mesh,
    gltfNodes :: Vector Node,
    gltfSamplers :: Vector Sampler,
    gltfTextures :: Vector Texture
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

-- | Image data used to create a texture.
data Image = Image
  { -- | The binary data of the image
    imageData :: Maybe ByteString,
    -- | The image’s media type.
    imageMimeType :: Text,
    -- | The user-defined name of this object.
    imageName :: Maybe Text
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
  { -- | The indices of this Node's children
    nodeChildren :: Vector Int,
    -- | The index of the mesh in this node.
    nodeMeshId :: Maybe Int,
    -- | The user-defined name of this object.
    nodeName :: Maybe Text,
    -- | The node's unit quaternion rotation.
    nodeRotation :: Maybe (Quaternion Float),
    -- | The node's non-uniform scale
    nodeScale :: Maybe (V3 Float),
    -- | The node's translation along the x, y, and z axes.
    nodeTranslation :: Maybe (V3 Float),
    -- | The weights of the instantiated morph target.
    nodeWeights :: [Float]
  } deriving (Eq, Show)

-- | Texture sampler properties for filtering and wrapping modes.
data Sampler = Sampler
  { -- | Magnification filter.
    samplerMagFilter :: Maybe MagFilter,
    -- | Minification filter.
    samplerMinFilter :: Maybe MinFilter,
    -- | The user-defined name of this object.
    samplerName :: Maybe Text,
    -- | S (U) wrapping mode. All valid values correspond to WebGL enums.
    samplerWrapS :: SamplerWrap,
    -- | T (V) wrapping mode. All valid values correspond to WebGL enums.
    samplerWrapT :: SamplerWrap
  } deriving (Eq, Show)

-- | A texture and its sampler.
data Texture = Texture
  { -- | The user-defined name of this object.
    textureName :: Maybe Text,
    -- | The index of the sampler used by this texture.
    textureSamplerId :: Maybe Int,
    -- | The index of the image used by this texture.
    textureSourceId :: Maybe Int
  } deriving (Eq, Show)

-- | Geometry to be rendered with the given material
data MeshPrimitive = MeshPrimitive
  { -- | A Vector of vertex indices.
    meshPrimitiveIndices :: Vector Word32,
    -- | The index of the material to apply to this primitive when rendering.
    meshPrimitiveMaterial :: Maybe Int,
    -- | The topology type of primitives to render.
    meshPrimitiveMode :: MeshPrimitiveMode,
    -- | A Vector of vertex normals.
    meshPrimitiveNormals :: Vector (V3 Float),
    -- | A Vector of vertex positions.
    meshPrimitivePositions :: Vector (V3 Float),
    -- | A Vector of vertex texture coordinates
    meshPrimitiveTexCoords :: Vector (V2 Float),
    -- | A Vector of vertex colors.
    meshPrimitiveColors :: Vector (V4 Float)
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
    -- | The base color texture
    pbrBaseColorTexture :: Maybe TextureInfo,
    -- | The factor for the metalness of the material.
    pbrMetallicFactor :: Float,
    -- | The factor for the roughness of the material.
    pbrRoughnessFactor :: Float
  } deriving (Eq, Show)

-- | Magnification filter.
data MagFilter = MagLinear | MagNearest
  deriving (Enum, Eq, Show)

-- | Minification Filter.
data MinFilter
  = MinNearest
  | MinLinear
  | MinNearestMipmapNearest
  | MinLinearMipmapNearest
  | MinNearestMipmapLinear
  | MinLinearMipmapLinear
  deriving (Enum, Eq, Show)

-- | Sampler wrapping mode.
data SamplerWrap
  = ClampToEdge
  | MirroredRepeat
  | Repeat
  deriving (Enum, Eq, Show)

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

-- | Reference to a texture.
data TextureInfo = TextureInfo
  { -- | The index of the texture.
    textureId :: Int,
    -- | This integer value is used to construct a string in the format
    -- TEXCOORD_<set_index> which is a reference to a key in mesh.primitives.attributes
    -- (e.g. a value of 0 corresponds to TEXCOORD_0).
    textureTexCoord :: Int
  } deriving (Eq, Show)

-- | Metadata about the glTF asset
_asset :: Lens' Gltf Asset
_asset = lens gltfAsset (\gltf asset -> gltf { gltfAsset = asset })

-- | A Vector of Images. An Image defines data used to create a texture.
_images :: Lens' Gltf (Vector Image)
_images = lens gltfImages (\gltf images -> gltf { gltfImages = images })

-- | A Vector of Materials. A Material defines the appearance of a primitive.
_materials :: Lens' Gltf (Vector Material)
_materials = lens gltfMaterials (\gltf mats -> gltf { gltfMaterials = mats })

-- | A Vector of Meshes. A Mesh is a set of primitives to be rendered.
_meshes :: Lens' Gltf (Vector Mesh)
_meshes = lens gltfMeshes (\gltf meshes -> gltf { gltfMeshes = meshes })

-- | A Vector of Nodes in the hierarchy.
_nodes :: Lens' Gltf (Vector Node)
_nodes = lens gltfNodes (\gltf nodes -> gltf { gltfNodes = nodes })

-- | A Vector of Texture Samplers. Texture Sampler defines properties for filtering and
-- wrapping modes.
_samplers :: Lens' Gltf (Vector Sampler)
_samplers = lens gltfSamplers (\gltf samplers -> gltf { gltfSamplers = samplers })

-- | A texture and its sampler.
_textures :: Lens' Gltf (Vector Texture)
_textures = lens gltfTextures (\gltf texs -> gltf { gltfTextures = texs })

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

-- | The binary data of the image
_imageData :: Lens' Image (Maybe ByteString)
_imageData = lens imageData (\img data' -> img { imageData = data' })

-- | The image’s media type.
_imageMimeType :: Lens' Image Text
_imageMimeType = lens imageMimeType (\img mime -> img { imageMimeType = mime })

-- | The user-defined name of this object.
_imageName :: Lens' Image (Maybe Text)
_imageName = lens imageName (\img name -> img { imageName = name })

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

-- | The indices of this node's children.
_nodeChildren :: Lens' Node (Vector Int)
_nodeChildren = lens nodeChildren (\node children -> node { nodeChildren = children })

-- | The index of the mesh in this node.
_nodeMeshId :: Lens' Node (Maybe Int)
_nodeMeshId = lens nodeMeshId (\node meshId -> node { nodeMeshId = meshId })

-- | The user-defined name of this object.
_nodeName :: Lens' Node (Maybe Text)
_nodeName = lens nodeName (\node name' -> node { nodeName = name' })

-- | The node's unit quaternion rotation.
_nodeRotation :: Lens' Node (Maybe (Quaternion Float))
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

-- | Magnification filter.
_samplerMagFilter :: Lens' Sampler (Maybe MagFilter)
_samplerMagFilter = lens
  samplerMagFilter
  (\sampler mag -> sampler { samplerMagFilter = mag })

-- | Minification filter.
_samplerMinFilter :: Lens' Sampler (Maybe MinFilter)
_samplerMinFilter = lens
  samplerMinFilter
  (\sampler min' -> sampler { samplerMinFilter = min' })

-- | The user-defined name of this object.
_samplerName :: Lens' Sampler (Maybe Text)
_samplerName = lens
  samplerName
  (\sampler name -> sampler { samplerName = name })

-- | S (U) wrapping mode.
_samplerWrapS :: Lens' Sampler SamplerWrap
_samplerWrapS = lens
  samplerWrapS
  (\sampler wrapS -> sampler { samplerWrapS = wrapS })

-- | T (V) wrapping mode.
_samplerWrapT :: Lens' Sampler SamplerWrap
_samplerWrapT = lens
  samplerWrapT
  (\sampler wrapT -> sampler { samplerWrapT = wrapT })

-- | The user-defined name of this object.
_textureName :: Lens' Texture (Maybe Text)
_textureName = lens textureName (\tex name -> tex { textureName = name })

-- | The index of the sampler used by this texture.
_textureSamplerId :: Lens' Texture (Maybe Int)
_textureSamplerId = lens
  textureSamplerId
  (\tex sampler -> tex { textureSamplerId = sampler })

-- | The index of the image used by this texture.
_textureSourceId :: Lens' Texture (Maybe Int)
_textureSourceId = lens
  textureSourceId
  (\tex source -> tex { textureSourceId = source })

-- | A Vector of vertex indices.
_meshPrimitiveIndices :: Lens' MeshPrimitive (Vector Word32)
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

-- | A Vector of vertex colors.
_meshPrimitiveColors :: Lens' MeshPrimitive (Vector (V4 Float))
_meshPrimitiveColors = lens
  meshPrimitiveColors
  (\primitive' colors -> primitive' { meshPrimitiveColors = colors })

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

-- | The base color texture
_pbrBaseColorTexture :: Lens' PbrMetallicRoughness (Maybe TextureInfo)
_pbrBaseColorTexture = lens
  pbrBaseColorTexture
  (\pbr texture -> pbr { pbrBaseColorTexture = texture })

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

-- | The index of the texture.
_textureInfoId :: Lens' TextureInfo Int
_textureInfoId = lens
  textureId
  (\texInfo id' -> texInfo { textureId = id' })

-- | This integer value is used to construct a string in the format
-- TEXCOORD_<set_index> which is a reference to a key in mesh.primitives.attributes
-- (e.g. a value of 0 corresponds to TEXCOORD_0).
_textureInfoTexCoord :: Lens' TextureInfo Int
_textureInfoTexCoord = lens
  textureTexCoord
  (\texInfo coord -> texInfo { textureTexCoord = coord })
