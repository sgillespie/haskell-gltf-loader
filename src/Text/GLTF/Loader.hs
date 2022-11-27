module Text.GLTF.Loader
  ( -- * Scene loading functions
    fromJsonFile,
    fromJsonByteString,
    fromBinaryFile,
    fromBinaryByteString,

    -- * GLTF Data Types
    module Text.GLTF.Loader.Gltf,
    module Text.GLTF.Loader.Glb,

    -- * Loading Errors
    module Text.GLTF.Loader.Errors
  ) where

import Text.GLTF.Loader.Adapter
import Text.GLTF.Loader.BufferAccessor
import Text.GLTF.Loader.Errors
import Text.GLTF.Loader.Glb
import Text.GLTF.Loader.Gltf

import Data.Binary.Get (ByteOffset)
import Data.Either
import Data.Maybe (fromJust)
import Lens.Micro
import RIO
import RIO.FilePath (takeDirectory)
import qualified Codec.GlTF as GlTF
import qualified Codec.GLB as GLB
import qualified RIO.Vector as Vector

-- | Load a glTF scene from a ByteString
fromJsonByteString :: MonadUnliftIO io => ByteString -> io (Either Errors Gltf)
fromJsonByteString input = toGltfResult "." Nothing (GlTF.fromByteString input)

-- | Load a glTF scene from a file
fromJsonFile :: MonadUnliftIO io => FilePath -> io (Either Errors Gltf)
fromJsonFile path = liftIO (GlTF.fromFile path) >>= toGltfResult (takeDirectory path) Nothing

fromBinaryFile :: MonadUnliftIO io => FilePath -> io (Either Errors Glb)
fromBinaryFile path = liftIO (GLB.fromFile path) >>= toGlbResult "."
  
fromBinaryByteString :: MonadUnliftIO io => ByteString -> io (Either Errors Glb)
fromBinaryByteString input = toGlbResult "." (GLB.fromByteString input)

toGltfResult
  :: MonadUnliftIO io
  => FilePath
  -> Maybe GLB.Chunk
  -> Either String GlTF.GlTF
  -> io (Either Errors Gltf)
toGltfResult basePath chunk res = res
  & over _Left (ReadError . fromString)
  & traverseOf _Right (runGltfAdapter basePath chunk)

toGlbResult
  :: MonadUnliftIO io
  => FilePath
  -> Either (ByteOffset, String) GLB.GLB
  -> io (Either Errors Glb)
toGlbResult basePath (Right res) = processGlb basePath res
toGlbResult _        (Left (_, err)) = pure . Left . ReadError . fromString $ err

runGltfAdapter
  :: MonadUnliftIO io
  => FilePath
  -> Maybe GLB.Chunk
  -> GlTF.GlTF
  -> io Gltf
runGltfAdapter basePath chunk gltf = runAdapter gltf
  <$> loadBuffers gltf chunk basePath
  <*> loadImages gltf basePath

processGlb :: MonadUnliftIO io => FilePath -> GLB.GLB -> io (Either Errors Glb)
processGlb basePath GLB.GLB{..} = over _Right Glb <$> res
  where gltfChunk = chunks Vector.!? 0
        bufferChunk = chunks Vector.!? 1
        gltf = GlTF.fromChunk (fromJust gltfChunk)
        res = toGltfResult basePath bufferChunk gltf
