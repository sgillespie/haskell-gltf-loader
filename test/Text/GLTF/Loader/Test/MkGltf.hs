module Text.GLTF.Loader.Test.MkGltf where

import Text.GLTF.Loader.Internal.Adapter

import Data.Binary.Put (putFloatle, putWord16le, runPut)

import Data.ByteString.Base64 (encodeBase64)
import Data.ByteString.Lazy (toStrict)
import Foreign (Storable(..))
import Linear
import RIO hiding (length)
import RIO.Text (length)
import qualified Codec.GLB as GLB
import qualified Codec.GlTF as GlTF
import qualified Codec.GlTF.Accessor as Accessor
import qualified Codec.GlTF.Asset as Asset
import qualified Codec.GlTF.Buffer as Buffer
import qualified Codec.GlTF.BufferView as BufferView
import qualified Codec.GlTF.Image as Image
import qualified Codec.GlTF.Material as Material
import qualified Codec.GlTF.Mesh as Mesh
import qualified Codec.GlTF.PbrMetallicRoughness as PbrMetallicRoughness
import qualified Codec.GlTF.Node as Node
import qualified Codec.GlTF.Sampler as Sampler
import qualified Codec.GlTF.Texture as Texture
import qualified Codec.GlTF.TextureInfo as TextureInfo
import qualified Codec.GlTF.URI as URI
import qualified Data.HashMap.Strict as HashMap

mkCodecGltf :: GlTF.GlTF
mkCodecGltf = GlTF.GlTF
  { asset = mkCodecAsset,
    extensionsUsed = Nothing,
    extensionsRequired = Nothing,
    accessors = Just
      [ mkCodecAccessorIndices,
        mkCodecAccessorPositions,
        mkCodecAccessorNormals,
        mkCodecAccessorTexCoords,
        mkCodecAccessorColors
      ],
    animations = Nothing,
    buffers = Just
      [ mkCodecBufferIndices,
        mkCodecBufferPositions,
        mkCodecBufferNormals,
        mkCodecBufferTexCoords,
        mkCodecBufferColors,
        mkCodecBufferImage
      ],
    bufferViews = Just
      [ mkCodecBufferViewIndices,
        mkCodecBufferViewPositions,
        mkCodecBufferViewNormals,
        mkCodecBufferViewTexCoords,
        mkCodecBufferViewColors,
        mkCodecBufferViewImage
      ],
    cameras = Nothing,
    images = Just [mkCodecImage],
    materials = Just [mkCodecMaterial],
    meshes = Just [mkCodecMesh],
    nodes = Just [mkCodecNode],
    samplers = Just [mkCodecSampler],
    scenes = Nothing,
    skins = Nothing,
    textures = Just [mkCodecTexture],
    extensions = Nothing,
    extras = Nothing
  }

mkCodecAsset :: Asset.Asset
mkCodecAsset = Asset.Asset
  { version = "version",
    copyright = Just "copyright",
    generator = Just "generator",
    minVersion = Just "minVersion",
    extensions = Nothing,
    extras = Nothing
  }

mkCodecAccessorIndices :: Accessor.Accessor
mkCodecAccessorIndices = Accessor.Accessor
  {
    bufferView = Just $ BufferView.BufferViewIx 0,
    byteOffset = 0,
    componentType = Accessor.ComponentType 5123,
    count = 4,
    extensions = Nothing,
    extras = Nothing,
    max = Nothing,
    min = Nothing,
    name = Just "Accessor Indices",
    normalized = False,
    sparse = Nothing,
    type' = Accessor.AttributeType "SCALAR"
  }

mkCodecAccessorNormals :: Accessor.Accessor
mkCodecAccessorNormals = Accessor.Accessor
  {
    bufferView = Just $ BufferView.BufferViewIx 2,
    byteOffset = 0,
    componentType = Accessor.ComponentType 5126,
    count = 4,
    extensions = Nothing,
    extras = Nothing,
    max = Nothing,
    min = Nothing,
    name = Just "Accessor Normals",
    normalized = False,
    sparse = Nothing,
    type' = Accessor.AttributeType "VEC3"
  }

mkCodecAccessorPositions :: Accessor.Accessor
mkCodecAccessorPositions = Accessor.Accessor
  {
    bufferView = Just $ BufferView.BufferViewIx 1,
    byteOffset = 0,
    componentType = Accessor.ComponentType 5126,
    count = 4,
    extensions = Nothing,
    extras = Nothing,
    max = Nothing,
    min = Nothing,
    name = Just "Accessor Positions",
    normalized = False,
    sparse = Nothing,
    type' = Accessor.AttributeType "VEC2"
  }

mkCodecAccessorTexCoords :: Accessor.Accessor
mkCodecAccessorTexCoords = Accessor.Accessor
  {
    bufferView = Just $ BufferView.BufferViewIx 3,
    byteOffset = 0,
    componentType = Accessor.ComponentType 5126,
    count = 4,
    extensions = Nothing,
    extras = Nothing,
    max = Nothing,
    min = Nothing,
    name = Just "Accessor Texture Coordinates",
    normalized = False,
    sparse = Nothing,
    type' = Accessor.AttributeType "VEC2"
  }

mkCodecAccessorColors :: Accessor.Accessor
mkCodecAccessorColors = Accessor.Accessor
  {
    bufferView = Just $ BufferView.BufferViewIx 4,
    byteOffset = 0,
    componentType = Accessor.ComponentType 5123,
    count = 4,
    extensions = Nothing,
    extras = Nothing,
    max = Nothing,
    min = Nothing,
    name = Just "Accessor Vertex Colors",
    normalized = False,
    sparse = Nothing,
    type' = Accessor.AttributeType "VEC3"
  }

mkCodecBufferViewIndices :: BufferView.BufferView
mkCodecBufferViewIndices = BufferView.BufferView
  { buffer = Buffer.BufferIx 0,
    byteOffset = 0,
    byteLength = sizeOf (undefined :: Word16) * 4,
    byteStride = Nothing,
    target = Nothing,
    name = Just "BufferView Indices",
    extensions = Nothing,
    extras = Nothing
  }

mkCodecBufferViewNormals :: BufferView.BufferView
mkCodecBufferViewNormals = BufferView.BufferView
  { buffer = Buffer.BufferIx 2,
    byteOffset = 0,
    byteLength = sizeOf (undefined :: V3 Float) * 4,
    byteStride = Nothing,
    target = Nothing,
    name = Just "BufferView Normals",
    extensions = Nothing,
    extras = Nothing
  }

mkCodecBufferViewPositions :: BufferView.BufferView
mkCodecBufferViewPositions = BufferView.BufferView
  { buffer = Buffer.BufferIx 1,
    byteOffset = 0,
    byteLength = sizeOf (undefined :: V3 Float) * 4,
    byteStride = Nothing,
    target = Nothing,
    name = Just "BufferView Positions",
    extensions = Nothing,
    extras = Nothing
  }

mkCodecBufferViewTexCoords :: BufferView.BufferView
mkCodecBufferViewTexCoords = BufferView.BufferView
  { buffer = Buffer.BufferIx 3,
    byteOffset = 0,
    byteLength = sizeOf (undefined :: V2 Float) * 4,
    byteStride = Nothing,
    target = Nothing,
    name = Just "BufferView Positions",
    extensions = Nothing,
    extras = Nothing
  }

mkCodecBufferViewColors :: BufferView.BufferView
mkCodecBufferViewColors = BufferView.BufferView
  { buffer = Buffer.BufferIx 4,
    byteOffset = 0,
    byteLength = sizeOf (undefined :: V4 Float) * 4,
    byteStride = Nothing,
    target = Nothing,
    name = Just "BufferView Colors",
    extensions = Nothing,
    extras = Nothing
  }

mkCodecBufferViewImage :: BufferView.BufferView
mkCodecBufferViewImage = BufferView.BufferView
  { buffer = Buffer.BufferIx 5,
    byteOffset = 0,
    byteLength = length "imageData" * 4,
    byteStride = Nothing,
    target = Nothing,
    name = Just "BufferView Image",
    extensions = Nothing,
    extras = Nothing
  }

mkCodecImage :: Image.Image
mkCodecImage = Image.Image
  { uri = Just (URI.URI "data:image/jpg;base64,aW1hZ2VQYXlsb2Fk"),
    mimeType = Just "image/png",
    bufferView = Just $ BufferView.BufferViewIx 5,
    name = Just "Image",
    extensions = Nothing,
    extras = Nothing
  }

mkCodecBufferIndices :: Buffer.Buffer
mkCodecBufferIndices = Buffer.Buffer
  { byteLength = sizeOf (undefined :: Word16) * 4,
    uri = Just mkCodecBufferUriIndices,
    extensions = Nothing,
    extras = Nothing,
    name = Just "Buffer Indices"
  }

mkCodecBufferNormals :: Buffer.Buffer
mkCodecBufferNormals = Buffer.Buffer
  { byteLength = sizeOf (undefined :: V3 Float) * 4,
    uri = Just mkCodecBufferUriNormals,
    extensions = Nothing,
    extras = Nothing,
    name = Just "Buffer Positions"
  }

mkCodecBufferPositions :: Buffer.Buffer
mkCodecBufferPositions = Buffer.Buffer
  { byteLength = sizeOf (undefined :: V3 Float) * 4,
    uri = Just mkCodecBufferUriPositions,
    extensions = Nothing,
    extras = Nothing,
    name = Just "Buffer Positions"
  }

mkCodecBufferTexCoords :: Buffer.Buffer
mkCodecBufferTexCoords = Buffer.Buffer
  { byteLength = sizeOf (undefined :: V2 Float) * 4,
    uri = Just mkCodecBufferUriTexCoords,
    extensions = Nothing,
    extras = Nothing,
    name = Just "Buffer Positions"
  }

mkCodecBufferColors :: Buffer.Buffer
mkCodecBufferColors = Buffer.Buffer
  { byteLength = sizeOf (undefined :: V4 Float) * 4,
    uri = Just mkCodecBufferUriColors,
    extensions = Nothing,
    extras = Nothing,
    name = Just "Buffer Colors"
  }

mkCodecBufferImage :: Buffer.Buffer
mkCodecBufferImage = Buffer.Buffer
  { byteLength = length "imageData" * 4,
    uri = Just mkCodecBufferUriImage,
    extensions = Nothing,
    extras = Nothing,
    name = Just "Buffer Image"
  }

mkCodecMaterial :: Material.Material
mkCodecMaterial = Material.Material
  { alphaCutoff = 1.0,
    emissiveFactor = (1.0, 2.0, 3.0),
    alphaMode = Material.OPAQUE,
    doubleSided = True,
    pbrMetallicRoughness = Just mkCodecPbrMetallicRoughness,
    normalTexture = Nothing,
    occlusionTexture = Nothing,
    emissiveTexture = Nothing,
    name = Just "Material",
    extensions = Nothing,
    extras = Nothing
  }

mkCodecMesh :: Mesh.Mesh
mkCodecMesh = Mesh.Mesh
  { primitives = [mkCodecMeshPrimitive],
    weights = Just [1.2],
    name = Just "mesh",
    extensions = Nothing,
    extras = Nothing
  }

mkCodecPbrMetallicRoughness :: PbrMetallicRoughness.PbrMetallicRoughness
mkCodecPbrMetallicRoughness = PbrMetallicRoughness.PbrMetallicRoughness
  { baseColorFactor = (1.0, 2.0, 3.0, 4.0),
    metallicFactor = 1.0,
    roughnessFactor = 2.0,
    metallicRoughnessTexture = Nothing,
    baseColorTexture = Just mkCodecBaseColorTexture,
    extensions = Nothing,
    extras = Nothing
  }

mkCodecBufferUriIndices :: URI.URI
mkCodecBufferUriIndices = URI.URI uriText
  where uriText = "data:application/octet-stream;base64," <> encodedText
        encodedText = encodeBase64 . toStrict . runPut $ putIndices
        putIndices = mapM_ putWord16le ([1..4] :: [Word16])

mkCodecBufferUriNormals :: URI.URI
mkCodecBufferUriNormals = URI.URI uriText
  where uriText = "data:application/octet-stream;base64," <> encodedText
        encodedText = encodeBase64. toStrict . runPut $ putPositions
        putPositions = mapM_ (replicateM_ 3 . putFloatle) ([5..8] :: [Float])

mkCodecBufferUriPositions :: URI.URI
mkCodecBufferUriPositions = URI.URI uriText
  where uriText = "data:application/octet-stream;base64," <> encodedText
        encodedText = encodeBase64. toStrict . runPut $ putPositions
        putPositions = mapM_ (replicateM_ 3 . putFloatle) ([1..4] :: [Float])

mkCodecBufferUriTexCoords :: URI.URI
mkCodecBufferUriTexCoords = URI.URI uriText
  where uriText = "data:application/octet-stream;base64," <> encodedText
        encodedText = encodeBase64. toStrict . runPut $ putPositions
        putPositions = mapM_ (replicateM_ 2 . putFloatle) ([9..12] :: [Float])

mkCodecBufferUriColors :: URI.URI
mkCodecBufferUriColors = URI.URI uriText
  where uriText = "data:application/octet-stream;base64," <> encodedText
        encodedText = encodeBase64. toStrict . runPut $ putPositions
        putPositions = mapM_ (replicateM_ 4 . putWord16le . toRatio) ([0, 0.2, 0.6, 1] :: [Float])
        toRatio w = floor $ w * fromIntegral (maxBound :: Word16)

mkCodecBufferUriImage :: URI.URI
mkCodecBufferUriImage = URI.URI uriText
  where uriText = "data:application/octet-stream;base64," <> encodedText
        encodedText = encodeBase64 . toStrict $ "imageData"

mkCodecBufferChunk :: GLB.Chunk
mkCodecBufferChunk = GLB.Chunk
  { chunkLength = fromIntegral $ sizeOf (undefined :: Word16) * 4,
    chunkType = 0x004E4942,
    chunkData = binary
  }
  where binary = toStrict . runPut $ putIndices
        putIndices = mapM_ putWord16le ([1..4] :: [Word16])

mkCodecMeshPrimitive :: Mesh.MeshPrimitive
mkCodecMeshPrimitive = Mesh.MeshPrimitive
  { attributes = HashMap.fromList
      [ (attributePosition, Accessor.AccessorIx 1),
        (attributeNormal, Accessor.AccessorIx 2),
        (attributeTexCoord, Accessor.AccessorIx 3)
      ],
    mode = Mesh.MeshPrimitiveMode 4,
    indices = Just $ Accessor.AccessorIx 0,
    material = Just $ Material.MaterialIx 1,
    targets = Nothing,
    extensions = Nothing,
    extras = Nothing
  }

mkCodecBaseColorTexture :: TextureInfo.TextureInfo_
mkCodecBaseColorTexture = TextureInfo.TextureInfo
  { extensions = Nothing,
    extras = Nothing,
    index = 15,
    subtype = TextureInfo.Basic,
    texCoord = 10
  }

mkCodecNode :: Node.Node
mkCodecNode = Node.Node
  { camera = Nothing,
    children = Just [Node.NodeIx 1],
    skin = Nothing,
    matrix = Nothing,
    mesh = Just (Mesh.MeshIx 5),
    rotation = Just (1, 2, 3, 4),
    scale = Just (5, 6, 7),
    translation = Just (8, 9, 10),
    weights = Just [11, 12, 13],
    name = Just "node",
    extensions = Nothing,
    extras = Nothing
  }

mkCodecSampler :: Sampler.Sampler
mkCodecSampler = Sampler.Sampler
  { extensions = Nothing,
    extras = Nothing,
    magFilter = Just Sampler.MAG_LINEAR,
    minFilter = Just Sampler.MIN_LINEAR,
    name = Just "Sampler",
    wrapS = Sampler.CLAMP_TO_EDGE,
    wrapT = Sampler.REPEAT
  }

mkCodecTexture :: Texture.Texture
mkCodecTexture = Texture.Texture
  { sampler = Just (Sampler.SamplerIx 0),
    source = Just (Image.ImageIx 0),
    name = Just "Texture",
    extensions = Nothing,
    extras = Nothing
  }
