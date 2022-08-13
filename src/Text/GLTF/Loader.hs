module Text.GLTF.Loader
  ( fromByteString,
    fromFile,
    
    module Text.GLTF.Loader.Errors,
    module Text.GLTF.Loader.Gltf
  ) where

import Text.GLTF.Loader.Adapter
import Text.GLTF.Loader.BufferAccessor
import Text.GLTF.Loader.Errors
import Text.GLTF.Loader.Gltf

import Data.Either
import Lens.Micro
import RIO
import qualified Codec.GlTF as GlTF

fromByteString :: MonadUnliftIO io => ByteString -> io (Either Errors Gltf)
fromByteString =  toGltfResult . GlTF.fromByteString

fromFile :: MonadUnliftIO io => FilePath -> io (Either Errors Gltf)
fromFile path = liftIO (GlTF.fromFile path) >>= toGltfResult
  
toGltfResult :: MonadUnliftIO io => Either String GlTF.GlTF -> io (Either Errors Gltf)
toGltfResult res
  = res
    & over _Left (ReadError . fromString)
    & traverseOf _Right toGltfResult'
  where toGltfResult' gltf = adaptGltf gltf <$> loadBuffers gltf
