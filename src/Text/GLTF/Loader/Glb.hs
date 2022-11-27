module Text.GLTF.Loader.Glb where

import RIO

import Text.GLTF.Loader.Gltf

newtype Glb = Glb { unGltf :: Gltf }
  deriving (Eq, Show)

_gltf :: Lens' Glb Gltf
_gltf = lens unGltf (const Glb)
