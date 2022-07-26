module Text.GLTF.Loader
  ( fromByteString,
    fromFile,
    
    module Text.GLTF.Loader.Errors,
    module Text.GLTF.Loader.Gltf
  ) where

import Text.GLTF.Loader.Adapter
import Text.GLTF.Loader.Errors
import Text.GLTF.Loader.Gltf

import Data.Either
import RIO
import RIO.Lens
import qualified Codec.GlTF as GlTF

fromByteString :: ByteString -> Either Errors Gltf
fromByteString =  toGltfResult . GlTF.fromByteString

fromFile :: FilePath -> IO (Either Errors Gltf)
fromFile path = toGltfResult <$> GlTF.fromFile path
  
toGltfResult :: Either String GlTF.GlTF -> Either Errors Gltf
toGltfResult res = res
  & over _Left (ReadError . fromString)
  & over _Right adaptGltf
