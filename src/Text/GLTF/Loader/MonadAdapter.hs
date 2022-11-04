module Text.GLTF.Loader.MonadAdapter
  ( Adapter(),
    AdaptEnv(..),
    getGltf,
    getBuffers,
    getImages
  ) where

import Text.GLTF.Loader.BufferAccessor

import RIO
import qualified Codec.GlTF as GlTF

type Adapter = Reader AdaptEnv

data AdaptEnv = AdaptEnv
  { asGltf :: GlTF.GlTF,
    asBuffers :: Vector GltfBuffer,
    asImages :: Vector GltfImageData
  }

getGltf :: Reader AdaptEnv GlTF.GlTF
getGltf = asks asGltf

getBuffers :: Reader AdaptEnv (Vector GltfBuffer)
getBuffers = asks asBuffers

getImages :: Reader AdaptEnv (Vector GltfImageData)
getImages = asks asImages
