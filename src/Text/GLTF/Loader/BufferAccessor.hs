module Text.GLTF.Loader.BufferAccessor
  ( GltfBuffer(..),
    loadBuffers,
    vertexIndices,
    getIndices,
    vertexPositions,
    vertexNormals
  ) where

import Codec.GlTF.Accessor
import Codec.GlTF.Buffer
import Codec.GlTF.BufferView
import Codec.GlTF.URI
import Codec.GlTF
import Data.ByteString.Lazy (fromStrict)
import Foreign.Storable
import Linear
import RIO hiding (min, max)
import qualified Data.Binary.Get as Binary
import qualified RIO.Vector as Vector
import qualified RIO.ByteString as ByteString

newtype GltfBuffer = GltfBuffer { unBuffer :: ByteString }
  deriving (Eq, Show)

loadBuffers :: MonadUnliftIO io => GlTF -> io (Vector GltfBuffer)
loadBuffers GlTF{buffers=buffers} = do
  let buffers' = fromMaybe [] buffers

  Vector.forM buffers' $ \Buffer{..} -> do
    payload <-
      maybe
      (return "")
      (\uri' -> do
          readRes <- liftIO $ loadURI undefined uri'
          case readRes of
            Left err -> error err
            Right res -> return res)
      uri
    
    return $ GltfBuffer payload

vertexIndices
  :: GlTF
  -> Vector GltfBuffer
  -> AccessorIx
  -> [Int]
vertexIndices GlTF{..} buffers' accessorId = fromMaybe [] $ do
  accessor <- lookupAccessor accessorId =<< accessors
  bufferView <- lookupBufferViewFromAccessor accessor =<< bufferViews
  buffer <- lookupBufferFromBufferView bufferView buffers'

  return $ readIndices accessor bufferView buffer

vertexPositions :: GlTF -> Vector GltfBuffer -> AccessorIx -> [V3 Float]
vertexPositions = undefined

vertexNormals :: GlTF -> Vector GltfBuffer -> AccessorIx -> [V3 Float]
vertexNormals = undefined

lookupBufferViewFromAccessor :: Accessor -> Vector BufferView -> Maybe BufferView
lookupBufferViewFromAccessor Accessor{..} bufferViews
  = bufferView >>= flip lookupBufferView bufferViews

lookupBufferFromBufferView :: BufferView -> Vector GltfBuffer -> Maybe GltfBuffer
lookupBufferFromBufferView BufferView{..} = lookupBuffer buffer

lookupAccessor :: AccessorIx -> Vector Accessor -> Maybe Accessor
lookupAccessor (AccessorIx accessorId) = (Vector.!? accessorId)

lookupBufferView :: BufferViewIx -> Vector BufferView -> Maybe BufferView
lookupBufferView (BufferViewIx bufferViewId) = (Vector.!? bufferViewId)

lookupBuffer :: BufferIx -> Vector GltfBuffer -> Maybe GltfBuffer
lookupBuffer (BufferIx bufferId) = (Vector.!? bufferId)

readIndices
  :: Accessor
  -> BufferView
  -> GltfBuffer
  -> [Int]
readIndices accessor bufferView (GltfBuffer payload)
  = readIndicesFromBuffer (offset + offset') count payload
  where Accessor{byteOffset=offset, count=count} = accessor
        BufferView{byteOffset=offset'} = bufferView

getIndices :: Binary.Get [Int]
getIndices = do
  empty <- Binary.isEmpty
  if empty
    then return []
    else do
      index <- getIndex
      indices <- getIndices
      return $ index : indices

readIndicesFromBuffer
  :: Int
  -> Int
  -> ByteString
  -> [Int]
readIndicesFromBuffer offset count payload
  = Binary.runGet getIndices (fromStrict indicesPayload)
  where indicesPayload = ByteString.take len' . ByteString.drop offset $ payload
        len' = count * sizeOf (undefined :: Word16)

getIndex :: Binary.Get Int
getIndex = fromIntegral <$> Binary.getWord16le
