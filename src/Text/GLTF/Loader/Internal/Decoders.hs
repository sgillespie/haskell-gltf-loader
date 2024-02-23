module Text.GLTF.Loader.Internal.Decoders
  ( -- * GLTF Property-specific Type decoders
    getIndices,
    getIndices32,
    getPositions,
    getNormals,
    getTexCoords,
    getColors,
    -- * GLTF Accessor Type decoders
    getScalar,
    getVec2,
    getVec3,
    getVec4,
    getMat2,
    getMat3,
    getMat4,
    -- * GLTF Component Type decoders
    getByte,
    getUnsignedByte,
    getShort,
    getUnsignedShort,
    getUnsignedInt,
    getFloat
  ) where

import qualified Codec.GlTF.Accessor as GlTF
import Data.Binary.Get
import Linear
import RIO hiding (min, max)
import qualified RIO.Vector as Vector

-- | Vertex indices binary decoder, for unsigned short
getIndices :: Get (Vector Word16)
getIndices = getScalar getUnsignedShort

-- | Vertex indices binary decoder, for unsigned int
getIndices32 :: Get (Vector Word32)
getIndices32 = getScalar getUnsignedInt

-- | Vertex positions binary decoder
getPositions :: Get (Vector (V3 Float))
getPositions = getVec3 getFloat

-- | Vertex normals binary decoder
getNormals :: Get (Vector (V3 Float))
getNormals = getVec3 getFloat

-- | Texture coordinates binary decoder
getTexCoords :: Get (Vector (V2 Float))
getTexCoords = getVec2 getFloat

-- | Vertex colors binary decoder
getColors :: Get (Vector (V4 Word16))
getColors = getVec4 getUnsignedShort

-- | Scalar (simple) type binary decoder
getScalar :: Get a -> Get (Vector a)
getScalar = getVector

-- | 2D Vector binary decoder
getVec2 :: Get a -> Get (Vector (V2 a))
getVec2 getter = getVector $ V2 <$> getter <*> getter

-- | 3D Vector binary decoder
getVec3 :: Get a -> Get (Vector (V3 a))
getVec3 getter = getVector $ V3 <$> getter <*> getter <*> getter

-- | 4D Vector binary decoder
getVec4 :: Get a -> Get (Vector (V4 a))
getVec4 getter = getVector $ V4 <$> getter <*> getter <*> getter <*> getter

-- | 2x2 Matrix binary decoder
getMat2 :: Get a -> Get (Vector (M22 a))
getMat2 getter = getVector $ do
  m1_1 <- getter
  m1_2 <- getter

  m2_1 <- getter
  m2_2 <- getter

  return $ V2
    (V2 m1_1 m2_1)
    (V2 m1_2 m2_2)

-- | 3x3 Matrix binary decoder
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

-- | 4x4 Matrix binary decoder
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

-- | Byte binary decoder
getByte :: Get Int8
getByte = getInt8

-- | Unsigned Byte binary decoder
getUnsignedByte :: Get Word8
getUnsignedByte = getWord8

-- | Short binary decoder
getShort :: Get Int16
getShort = getInt16le

-- | Unsigned Short binary decoder
getUnsignedShort :: Get Word16
getUnsignedShort = getWord16le

-- | Unsigned Int binary decoder
getUnsignedInt :: Get Word32
getUnsignedInt = getWord32le

-- | Float binary decoder
getFloat :: Get Float
getFloat = getFloatle

-- | Boxed Vector binary decoder
getVector :: Get a -> Get (Vector a)
getVector = fmap Vector.fromList . getList

-- | List binary decoder
getList :: Get a -> Get [a]
getList getter = do
  empty <- isEmpty
  if empty
    then return []
    else (:) <$> getter <*> getList getter
