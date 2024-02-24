module Text.GLTF.Loader.Glb
  ( Glb (..),
    _gltf,
  ) where

import RIO

import Text.GLTF.Loader.Gltf

-- | A Binary glTF
newtype Glb = Glb {unGltf :: Gltf}
  deriving (Eq, Show)

-- | Embedded glTF root data, from the first chunk
_gltf :: Lens' Glb Gltf
_gltf = lens unGltf (const Glb)
