module Text.GLTF.Loader.BufferAccessor
  ( GltfBuffer(..),
    -- * Loading GLTF buffers
    loadBuffers,
    -- * Deserializing Accessors
    vertexIndices,
    vertexPositions,
    vertexNormals,
    vertexTexCoords,
    -- * Binary Get decoders
    -- ** Specific Type decoders
    getIndices,
    getPositions,
    -- ** GLTF Accessor Type decoders
    getScalar,
    getVec2,
    getVec3,
    getVec4,
    getMat2,
    getMat3,
    getMat4,
    -- ** GLTF Component Type decoders
    getByte,
    getUnsignedByte,
    getShort,
    getUnsignedShort,
    getUnsignedInt,
    getFloat
  ) where

import Codec.GlTF.Accessor
import Codec.GlTF.Buffer
import Codec.GlTF.BufferView
import Codec.GlTF.URI
import Codec.GlTF
import Data.Binary.Get
import Data.ByteString.Lazy (fromStrict)
import Foreign.Storable
import Linear
import RIO hiding (min, max)
import qualified RIO.Vector as Vector
import qualified RIO.ByteString as ByteString

newtype GltfBuffer = GltfBuffer { unBuffer :: ByteString }
  deriving (Eq, Show)

data BufferAccessor = BufferAccessor
  { offset :: Int,
    count :: Int,
    buffer :: GltfBuffer
  }

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

vertexIndices :: GlTF -> Vector GltfBuffer -> AccessorIx -> Vector Int
vertexIndices = readBufferWithGet getIndices

vertexPositions :: GlTF -> Vector GltfBuffer -> AccessorIx -> Vector (V3 Float)
vertexPositions = readBufferWithGet getPositions

vertexNormals :: GlTF -> Vector GltfBuffer -> AccessorIx -> Vector (V3 Float)
vertexNormals = readBufferWithGet getNormals

vertexTexCoords :: GlTF -> Vector GltfBuffer -> AccessorIx -> Vector (V2 Float)
vertexTexCoords = readBufferWithGet getTexCoords

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

readFromBuffer
  :: Storable storable
  => storable
  -> Get (Vector storable)
  -> BufferAccessor
  -> Vector storable
readFromBuffer storable getter BufferAccessor{..}
  = runGet getter (fromStrict payload')
  where payload' = ByteString.take len' . ByteString.drop offset . unBuffer $ buffer
        len' = count * sizeOf storable

getIndices :: Get (Vector Int)
getIndices = getScalar (fromIntegral <$> getUnsignedShort)

getPositions :: Get (Vector (V3 Float))
getPositions = getVec3 getFloat

getNormals :: Get (Vector (V3 Float))
getNormals = getVec3 getFloat

getTexCoords :: Get (Vector (V2 Float))
getTexCoords = getVec2 getFloat

getScalar :: Get a -> Get (Vector a)
getScalar = getVector

getVec2 :: Get a -> Get (Vector (V2 a))
getVec2 getter = getVector $ V2 <$> getter <*> getter

getVec3 :: Get a -> Get (Vector (V3 a))
getVec3 getter = getVector $ V3 <$> getter <*> getter <*> getter

getVec4 :: Get a -> Get (Vector (V4 a))
getVec4 getter = getVector $ V4 <$> getter <*> getter <*> getter <*> getter

getMat2 :: Get a -> Get (Vector (M22 a))
getMat2 getter = getVector $ do
  m1_1 <- getter
  m1_2 <- getter

  m2_1 <- getter
  m2_2 <- getter

  return $ V2
    (V2 m1_1 m2_1)
    (V2 m1_2 m2_2)

getMat3 :: Get a -> Get (Vector (M33 a))
getMat3 getter = getVector $ do
  m1_1 <- getter
  m1_2 <- getter
  m1_3 <- getter
  
  m2_1 <- getter
  m2_2 <- getter
  m2_3 <- getter

  m3_1 <- getter
  m3_2 <- getter
  m3_3 <- getter

  return $ V3
    (V3 m1_1 m2_1 m3_1)
    (V3 m1_2 m2_2 m3_2)
    (V3 m1_3 m2_3 m3_3)

getMat4 :: Get a -> Get (Vector (M44 a))
getMat4 getter = getVector $ do
  m1_1 <- getter
  m1_2 <- getter
  m1_3 <- getter
  m1_4 <- getter
  
  m2_1 <- getter
  m2_2 <- getter
  m2_3 <- getter
  m2_4 <- getter

  m3_1 <- getter
  m3_2 <- getter
  m3_3 <- getter
  m3_4 <- getter

  m4_1 <- getter
  m4_2 <- getter
  m4_3 <- getter
  m4_4 <- getter

  return $ V4
    (V4 m1_1 m2_1 m3_1 m4_1)
    (V4 m1_2 m2_2 m3_2 m4_2)
    (V4 m1_3 m2_3 m3_3 m4_3)
    (V4 m1_4 m2_4 m3_4 m4_4)

getByte :: Get Int8
getByte = getInt8

getUnsignedByte :: Get Word8
getUnsignedByte = getWord8

getShort :: Get Int16
getShort = getInt16le

getUnsignedShort :: Get Word16
getUnsignedShort = getWord16le

getUnsignedInt :: Get Word32
getUnsignedInt = getWord32le

getFloat :: Get Float
getFloat = getFloatle

getVector :: Get a -> Get (Vector a)
getVector = fmap Vector.fromList . getList

getList :: Get a -> Get [a]
getList getter = do
  empty <- isEmpty
  if empty
    then return []
    else (:) <$> getter <*> getList getter
