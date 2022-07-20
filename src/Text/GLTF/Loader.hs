module Text.GLTF.Loader where

import RIO

data Errors = ImpossibleError
  deriving (Show, Eq, Typeable)

data Gltf = Gltf
  deriving (Eq, Show)

fromByteString :: ByteString -> Either Errors Gltf
fromByteString _ = Left ImpossibleError
