module Text.GLTF.Loader.Test.MkGltf where

import Text.GLTF.Loader.Adapter (attributePosition)

import Data.Binary.Builder
import Data.Binary.Put (putFloatle, runPut)
import Data.ByteString.Base64 (encodeBase64)
import Data.ByteString.Lazy (toStrict)
import Foreign (Storable(..))
import Linear (V3(..))
import RIO
import qualified Codec.GlTF as GlTF
import qualified Codec.GlTF.Accessor as Accessor
import qualified Codec.GlTF.Asset as Asset
import qualified Codec.GlTF.Buffer as Buffer
import qualified Codec.GlTF.BufferView as BufferView
import qualified Codec.GlTF.Mesh as Mesh
import qualified Codec.GlTF.Node as Node
import qualified Codec.GlTF.URI as URI
import qualified Data.HashMap.Strict as HashMap

mkCodecGltf :: GlTF.GlTF
mkCodecGltf = GlTF.GlTF
  { asset = mkCodecAsset,
    extensionsUsed = Nothing,
    extensionsRequired = Nothing,
    accessors = Just [mkCodecAccessorIndices, mkCodecAccessorPositions],
    animations = Nothing,
    buffers = Just [mkCodecBufferIndices, mkCodecBufferPositions],
    bufferViews = Just [mkCodecBufferViewIndices, mkCodecBufferViewPositions],
    cameras = Nothing,
    images = Nothing,
    materials = Nothing,
    meshes = Just [mkCodecMesh],
    nodes = Just [mkCodecNode],
    samplers = Nothing,
    scenes = Nothing,
    skins = Nothing,
    textures = Nothing,
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

mkCodecBufferIndices :: Buffer.Buffer
mkCodecBufferIndices = Buffer.Buffer
  { byteLength = sizeOf (undefined :: Word16) * 4,
    uri = Just mkCodecBufferUriIndices,
    extensions = Nothing,
    extras = Nothing,
    name = Just "Buffer Indices"
  }

mkCodecBufferPositions :: Buffer.Buffer
mkCodecBufferPositions = Buffer.Buffer
  { byteLength = sizeOf (undefined :: V3 Float) * 4,
    uri = Just mkCodecBufferUriPositions,
    extensions = Nothing,
    extras = Nothing,
    name = Just "Buffer Positions"
  }

mkCodecMesh :: Mesh.Mesh
mkCodecMesh = Mesh.Mesh
  { primitives = [mkCodecMeshPrimitive],
    weights = Just [1.2],
    name = Just "mesh",
    extensions = Nothing,
    extras = Nothing
  }

mkCodecBufferUriIndices :: URI.URI
mkCodecBufferUriIndices = URI.URI uriText
  where uriText = "data:application/octet-stream;base64," <> encodedText
        encodedText = encodeBase64. toStrict . toLazyByteString $ putIndices
        putIndices = foldr ((<>) . putWord16le) empty ([1..4] :: [Word16])

mkCodecBufferUriPositions :: URI.URI
mkCodecBufferUriPositions = URI.URI uriText
  where uriText = "data:application/octet-stream;base64," <> encodedText
        encodedText = encodeBase64. toStrict . runPut $ putPositions
        putPositions = mapM_ (replicateM_ 3 . putFloatle) ([1..4] :: [Float])

mkCodecMeshPrimitive :: Mesh.MeshPrimitive
mkCodecMeshPrimitive = Mesh.MeshPrimitive
  { attributes = HashMap.fromList
      [(attributePosition, Accessor.AccessorIx 1)],
    mode = Mesh.MeshPrimitiveMode 4,
    indices = Just $ Accessor.AccessorIx 0,
    material = Nothing,
    targets = Nothing,
    extensions = Nothing,
    extras = Nothing
  }

mkCodecNode :: Node.Node
mkCodecNode = Node.Node
  { camera = Nothing,
    children = Nothing,
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