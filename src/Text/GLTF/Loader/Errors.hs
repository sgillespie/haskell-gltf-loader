module Text.GLTF.Loader.Errors
  ( Errors(..),
    _ReadError,
    _ImpossibleError
  ) where

import Lens.Micro (Traversal'())
import RIO

data Errors
  = ReadError Text
  | ImpossibleError
  deriving (Show, Eq, Typeable)

instance Display Errors where
  textDisplay = displayErrorText

_ReadError :: Traversal' Errors Text
_ReadError f (ReadError text) = ReadError <$> f text
_ReadError _ err = pure err

_ImpossibleError :: Traversal' Errors ()
_ImpossibleError f ImpossibleError = f () $> ImpossibleError
_ImpossibleError _ err = pure err

displayErrorText :: Errors -> Text
displayErrorText (ReadError txt) = txt
displayErrorText ImpossibleError = "An impossible error has occurred!"
