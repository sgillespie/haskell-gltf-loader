module Text.GLTF.Loader.BufferAccessor
  ( GltfBuffer(..),
    GltfImageData(..),
    -- * Loading GLTF buffers
    loadBuffers,
    loadImages,
    -- * Deserializing Accessors
    vertexIndices,
    vertexPositions,
    vertexNormals,
    vertexTexCoords,
    imageDataRaw,
  ) where

import Text.GLTF.Loader.Decoders

import Codec.GlTF.Accessor
import Codec.GlTF.Buffer
import Codec.GlTF.BufferView
import Codec.GlTF.Image
import Codec.GlTF.URI
import Codec.GlTF
import Data.Binary.Get
import Data.ByteString.Lazy (fromStrict)
import Foreign.Storable
import Linear
import RIO hiding (min, max)
import qualified RIO.Vector as Vector
import qualified RIO.ByteString as ByteString

-- | Holds the entire payload of a glTF buffer
newtype GltfBuffer = GltfBuffer { unBuffer :: ByteString }
  deriving (Eq, Show)

data GltfImageData
  = ImageData ByteString
  | ImageBufferView BufferViewIx
  | NoImageData
  deriving (Eq, Show)

-- | A buffer and some metadata
data BufferAccessor = BufferAccessor
  { offset :: Int,
    count :: Int,
    buffer :: GltfBuffer
  }

-- | Read all the buffers into memory
loadBuffers :: MonadUnliftIO io => GlTF -> io (Vector GltfBuffer)
loadBuffers GlTF{buffers=buffers} = do
  let buffers' = fromMaybe [] buffers

  Vector.forM buffers' $ \Buffer{..} ->
    GltfBuffer <$> maybe (return mempty) loadUri' uri

loadImages :: MonadUnliftIO io => GlTF -> io (Vector GltfImageData)
loadImages GlTF{images=images} = do
  let images' = fromMaybe [] images

  Vector.forM images' $ \Image{..} ->
    let fallbackImageData = return $ maybe NoImageData ImageBufferView bufferView
    in maybe fallbackImageData (fmap ImageData . loadUri') uri

-- | Decode vertex indices
vertexIndices :: GlTF -> Vector GltfBuffer -> AccessorIx -> Vector Word16
vertexIndices = readBufferWithGet getIndices

-- | Decode vertex positions
vertexPositions :: GlTF -> Vector GltfBuffer -> AccessorIx -> Vector (V3 Float)
vertexPositions = readBufferWithGet getPositions

-- | Decode vertex normals
vertexNormals :: GlTF -> Vector GltfBuffer -> AccessorIx -> Vector (V3 Float)
vertexNormals = readBufferWithGet getNormals

-- | Decode texture coordinates. Note that we only use the first one.
vertexTexCoords :: GlTF -> Vector GltfBuffer -> AccessorIx -> Vector (V2 Float)
vertexTexCoords = readBufferWithGet getTexCoords

-- | Read an image from a buffer view
imageDataRaw :: GlTF -> Vector GltfBuffer -> BufferViewIx -> Maybe ByteString
imageDataRaw = readBufferView

-- | Return a buffer view undecoded
readBufferView :: GlTF -> Vector GltfBuffer -> BufferViewIx -> Maybe ByteString
readBufferView gltf buffers' bufferViewId = do
  accessor@BufferAccessor{count=length'}
    <- bufferViewAccessor gltf buffers' bufferViewId

  return $ readFromBufferRaw accessor length'

-- | Read a URI. Throws error on failure
loadUri' :: MonadUnliftIO io => URI -> io ByteString
loadUri' uri' = do
  readRes <- liftIO $ loadURI undefined uri'
  case readRes of
    Left err -> error err
    Right res -> return res

-- | Decode a buffer using the given Binary decoder
readBufferWithGet
  :: Storable storable
  => Get (Vector storable)
  -> GlTF
  -> Vector GltfBuffer
  -> AccessorIx
  -> Vector storable
readBufferWithGet getter gltf buffers' accessorId
  = maybe mempty
      (readFromBuffer undefined getter)
      (bufferAccessor gltf buffers' accessorId)

-- | Look up a Buffer from a GlTF and AccessorIx
bufferAccessor
  :: GlTF
  -> Vector GltfBuffer
  -> AccessorIx
  -> Maybe BufferAccessor
bufferAccessor GlTF{..} buffers' accessorId = do
  accessor <- lookupAccessor accessorId =<< accessors
  bufferView <- lookupBufferViewFromAccessor accessor =<< bufferViews
  buffer <- lookupBufferFromBufferView bufferView buffers'

  let Accessor{byteOffset=offset, count=count} = accessor
      BufferView{byteOffset=offset'} = bufferView

  return $ BufferAccessor
    { offset = offset + offset',
      count = count,
      buffer = buffer
    }

-- | Look up a Buffer from a GlTF and BufferView
bufferViewAccessor
  :: GlTF
  -> Vector GltfBuffer
  -> BufferViewIx
  -> Maybe BufferAccessor
bufferViewAccessor GlTF{..} buffers' bufferViewId = do
  bufferView <- lookupBufferView bufferViewId =<< bufferViews
  buffer <- lookupBufferFromBufferView bufferView buffers'

  let BufferView{byteLength=length', byteOffset=offset'} = bufferView

  return $ BufferAccessor
    { offset = offset',
      count = length',
      buffer = buffer
    }

-- | Look up a BufferView by Accessor
lookupBufferViewFromAccessor :: Accessor -> Vector BufferView -> Maybe BufferView
lookupBufferViewFromAccessor Accessor{..} bufferViews
  = bufferView >>= flip lookupBufferView bufferViews

-- | Look up a Buffer by BufferView
lookupBufferFromBufferView :: BufferView -> Vector GltfBuffer -> Maybe GltfBuffer
lookupBufferFromBufferView BufferView{..} = lookupBuffer buffer

-- | Look up an Accessor by Ix
lookupAccessor :: AccessorIx -> Vector Accessor -> Maybe Accessor
lookupAccessor (AccessorIx accessorId) = (Vector.!? accessorId)

-- | Look up a BufferView by Ix
lookupBufferView :: BufferViewIx -> Vector BufferView -> Maybe BufferView
lookupBufferView (BufferViewIx bufferViewId) = (Vector.!? bufferViewId)

-- | Look up a Buffer by Ix
lookupBuffer :: BufferIx -> Vector GltfBuffer -> Maybe GltfBuffer
lookupBuffer (BufferIx bufferId) = (Vector.!? bufferId)

-- | Decode a buffer using the given Binary decoder
readFromBuffer
  :: Storable storable
  => storable
  -> Get (Vector storable)
  -> BufferAccessor
  -> Vector storable
readFromBuffer storable getter accessor@BufferAccessor{..}
  = runGet getter . fromStrict $ payload
  where payload = readFromBufferRaw accessor len'
        len' = count * sizeOf storable

-- | Read from buffer without decoding
readFromBufferRaw :: BufferAccessor -> Int -> ByteString
readFromBufferRaw BufferAccessor{..} len'
  = ByteString.take len' . ByteString.drop offset . unBuffer $ buffer
