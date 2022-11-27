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

import Data.Either
import Lens.Micro
import RIO
import RIO.FilePath (takeDirectory)
import qualified Codec.GlTF as GlTF
import qualified Codec.GLB as GLB
import qualified RIO.Vector.Partial as Vector

-- | Load a glTF scene from a ByteString
fromJsonByteString :: MonadUnliftIO io => ByteString -> io (Either Errors Gltf)
fromJsonByteString input = toGltfResult "." Nothing (GlTF.fromByteString input)

-- | Load a glTF scene from a file
fromJsonFile :: MonadUnliftIO io => FilePath -> io (Either Errors Gltf)
fromJsonFile path = liftIO (GlTF.fromFile path) >>= toGltfResult (takeDirectory path) Nothing

fromBinaryFile :: MonadUnliftIO io => FilePath -> io (Either Errors Glb)
fromBinaryFile path = do
  res <- liftIO $ GLB.fromFile path
  case res of
    Left (_, err) -> pure . Left . ReadError . fromString $ err
    Right GLB.GLB{..} -> do
      let gltfChunk = Vector.head chunks
          gltf = GlTF.fromChunk gltfChunk

      res' <- toGltfResult "." (Just (chunks Vector.! 1)) gltf
      pure $ over _Right Glb res'

fromBinaryByteString :: MonadUnliftIO io => ByteString -> io (Either Errors Glb)
fromBinaryByteString input = do
  let res = GLB.fromByteString input
  case res of
    Left (_, err) -> pure . Left . ReadError . fromString $ err
    Right GLB.GLB{..} -> do
      let gltfChunk = Vector.head chunks
          gltf = GlTF.fromChunk gltfChunk

      res' <- toGltfResult "." (Just (chunks Vector.! 1)) gltf
      pure $ over _Right Glb res'

toGltfResult
  :: MonadUnliftIO io
  => FilePath
  -> Maybe GLB.Chunk
  -> Either String GlTF.GlTF
  -> io (Either Errors Gltf)
toGltfResult basePath chunk res = res
  & over _Left (ReadError . fromString)
  & traverseOf _Right toGltfResult'
  where toGltfResult' gltf
          = runAdapter gltf
          <$> loadBuffers gltf chunk basePath
          <*> loadImages gltf basePath

