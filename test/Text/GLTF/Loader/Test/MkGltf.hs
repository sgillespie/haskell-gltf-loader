module Text.GLTF.Loader.Test.MkGltf where

import Data.Binary.Builder
import Data.ByteString.Base64 (encodeBase64)
import Data.ByteString.Lazy (toStrict)
import Foreign (Storable(..))
import RIO
import qualified Codec.GlTF as GlTF
import qualified Codec.GlTF.Accessor as Accessor
import qualified Codec.GlTF.Asset as Asset
import qualified Codec.GlTF.Buffer as Buffer
import qualified Codec.GlTF.BufferView as BufferView
import qualified Codec.GlTF.URI as URI

mkGltf :: GlTF.GlTF
mkGltf = GlTF.GlTF
  { asset = mkAsset,
    extensionsUsed = Nothing,
    extensionsRequired = Nothing,
    accessors = Just [mkAccessor],
    animations = Nothing,
    buffers = Just [mkBuffer],
    bufferViews = Just [mkBufferView],
    cameras = Nothing,
    images = Nothing,
    materials = Nothing,
    meshes = Nothing,
    nodes = Nothing,
    samplers = Nothing,
    scenes = Nothing,
    skins = Nothing,
    textures = Nothing,
    extensions = Nothing,
    extras = Nothing
  }

mkAsset :: Asset.Asset
mkAsset = Asset.Asset
  { version = "version",
    copyright = Just "copyright",
    generator = Just "generator",
    minVersion = Just "minVersion",
    extensions = Nothing,
    extras = Nothing
  }

mkAccessor :: Accessor.Accessor
mkAccessor = Accessor.Accessor
  {
    bufferView = Just $ BufferView.BufferViewIx 0,
    byteOffset = 0,
    componentType = Accessor.ComponentType 5123,
    count = 4,
    extensions = Nothing,
    extras = Nothing,
    max = Nothing,
    min = Nothing,
    name = Just "Accessor",
    normalized = False,
    sparse = Nothing,
    type' = Accessor.AttributeType "SCALAR"
  }

mkBufferView :: BufferView.BufferView
mkBufferView = BufferView.BufferView
  { buffer = Buffer.BufferIx 0,
    byteOffset = 0,
    byteLength = sizeOf (undefined :: Word16) * 4,
    byteStride = Nothing,
    target = Nothing,
    name = Just "BufferView",
    extensions = Nothing,
    extras = Nothing
  }

mkBuffer :: Buffer.Buffer
mkBuffer = Buffer.Buffer
  { byteLength = sizeOf (undefined :: Word16) * 4,
    uri = Just mkBufferUri,
    extensions = Nothing,
    extras = Nothing,
    name = Just "Buffer"
  }

mkBufferUri :: URI.URI
mkBufferUri = URI.URI uriText
  where uriText = "data:application/octet-stream;base64," <> encodedText
        encodedText = encodeBase64. toStrict . toLazyByteString $ putIndices
        putIndices = foldr ((<>) . putWord16le) empty ([1..4] :: [Word16])
