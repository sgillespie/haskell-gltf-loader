module Text.GLTF.Loader
  ( -- * Scene loading functions
    fromFile,
    fromByteString,

    -- * GLTF Data Types
    module Text.GLTF.Loader.Gltf,

    -- * Loading Errors
    module Text.GLTF.Loader.Errors
  ) where

import Text.GLTF.Loader.Adapter
import Text.GLTF.Loader.BufferAccessor
import Text.GLTF.Loader.Errors
import Text.GLTF.Loader.Gltf

import Data.Either
import Lens.Micro
import RIO
import qualified Codec.GlTF as GlTF

-- | Load a glTF scene from a ByteString
fromByteString :: MonadUnliftIO io => ByteString -> io (Either Errors Gltf)
fromByteString =  toGltfResult . GlTF.fromByteString

-- | Load a glTF scene from a file
fromFile :: MonadUnliftIO io => FilePath -> io (Either Errors Gltf)
fromFile path = liftIO (GlTF.fromFile path) >>= toGltfResult
  
toGltfResult :: MonadUnliftIO io => Either String GlTF.GlTF -> io (Either Errors Gltf)
toGltfResult res
  = res
    & over _Left (ReadError . fromString)
    & traverseOf _Right toGltfResult'
   where toGltfResult' gltf = adaptGltf gltf <$> loadBuffers gltf
