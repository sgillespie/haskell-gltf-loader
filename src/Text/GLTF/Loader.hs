module Text.GLTF.Loader
  ( fromByteString,
    fromFile,
    
    module Text.GLTF.Loader.Errors,
    module Text.GLTF.Loader.Gltf
  ) where

import Text.GLTF.Loader.Errors
import Text.GLTF.Loader.Gltf

import Data.Either
import RIO
import RIO.Lens
import qualified Codec.GlTF as Gltf

fromByteString :: ByteString -> Either Errors Gltf
fromByteString =  toGltfResult . Gltf.fromByteString

fromFile :: FilePath -> IO (Either Errors Gltf)
fromFile path = toGltfResult <$> Gltf.fromFile path
  
toGltfResult :: Either String Gltf.GlTF -> Either Errors Gltf
toGltfResult res = res
  & over _Left (ReadError . fromString)
  & over _Right transformGltf

transformGltf :: Gltf.GlTF -> Gltf
transformGltf Gltf.GlTF{..} = Gltf nodes'
  where nodes' = maybe [] (map (const Node) . toList) nodes
