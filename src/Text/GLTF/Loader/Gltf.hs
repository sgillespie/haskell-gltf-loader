module Text.GLTF.Loader.Gltf
  ( Gltf(..),
    Node(..)
  ) where

import RIO

data Gltf = Gltf
  { nodes :: [Node] }
  deriving (Eq, Show)

data Node = Node
  deriving (Eq, Show)
