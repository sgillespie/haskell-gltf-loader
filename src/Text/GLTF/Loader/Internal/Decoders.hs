{-# HLINT ignore "Use <$>" #-}

module Text.GLTF.Loader.Internal.Decoders
  ( -- * GLTF Property-specific Type decoders
    getIndices,
    getIndices32,
    getPositions,
    getNormals,
    getTangents,
    getTexCoords,
    getColors,
    getJoints,
    getJoints16,
    getWeights,
    getWeights8,
    getWeights16,
    getInverseBindMatrices,

    -- * GLTF Accessor Type decoders
    getScalar,
    getVec2,
    getVec3,
    getVec4,
    getMat2,
    getMat3,
    getMat4,
    getQuaternion,

    -- * GLTF Component Type decoders
    getByte,
    getUnsignedByte,
    getShort,
    getUnsignedShort,
    getUnsignedInt,
    getFloat,
  ) where

import Data.Binary.Get
import Linear
import RIO hiding (max, min)
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

-- | Vertex tangents binary decoder
getTangents :: Get (Vector (V4 Float))
getTangents = getVec4 getFloat

-- | Texture coordinates binary decoder
getTexCoords :: Get (Vector (V2 Float))
getTexCoords = getVec2 getFloat

-- | Vertex colors binary decoder
getColors :: Get (Vector (V4 Word16))
getColors = getVec4 getUnsignedShort

-- | Vertex joints binary decoder, for unsigned bytes
getJoints :: Get (Vector (V4 Word8))
getJoints = getVec4 getUnsignedByte

-- | Vertex joints binary decoder, for unsigned shorts
getJoints16 :: Get (Vector (V4 Word16))
getJoints16 = getVec4 getUnsignedShort

-- | Vertex joint weights binary decoder, for floats
getWeights :: Get (Vector (V4 Float))
getWeights = getVec4 getFloat

-- | Vertex weights binary decoder, for unsigned bytes
getWeights8 :: Get (Vector (V4 Word8))
getWeights8 = getVec4 getUnsignedByte

-- | Vertex weights binary decoder, for unsigned shorts
getWeights16 :: Get (Vector (V4 Word16))
getWeights16 = getVec4 getUnsignedShort

-- | Inverse bind matrix decoder
getInverseBindMatrices :: Get (Vector (M44 Float))
getInverseBindMatrices = getMat4 getFloat

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

-- The `getMat*` functions below use a style not recognized by Fourmolu
{- FOURMOLU_DISABLE -}

-- | 2x2 Matrix binary decoder
getMat2 :: Get a -> Get (Vector (M22 a))
getMat2 getter =
  getVector $ do {
    m11 <- getter; m12 <- getter;
    m21 <- getter; m22 <- getter;
    return $ V2
      (V2 m11 m21)
      (V2 m12 m22)
  }

-- | 3x3 Matrix binary decoder
getMat3 :: Get a -> Get (Vector (M33 a))
getMat3 getter =
  getVector $ do {
    m11 <- getter; m12 <- getter; m13 <- getter;
    m21 <- getter; m22 <- getter; m23 <- getter;
    m31 <- getter; m32 <- getter; m33 <- getter;
    return $ V3
      (V3 m11 m21 m31)
      (V3 m12 m22 m32)
      (V3 m13 m23 m33)
  }

-- | 4x4 Matrix binary decoder
getMat4 :: Get a -> Get (Vector (M44 a))
getMat4 getter =
  getVector $ do {
    m11 <- getter; m12 <- getter; m13 <- getter; m14 <- getter;
    m21 <- getter; m22 <- getter; m23 <- getter; m24 <- getter;
    m31 <- getter; m32 <- getter; m33 <- getter; m34 <- getter;
    m41 <- getter; m42 <- getter; m43 <- getter; m44 <- getter;
    return $ V4
      (V4 m11 m21 m31 m41)
      (V4 m12 m22 m32 m42)
      (V4 m13 m23 m33 m43)
      (V4 m14 m24 m34 m44)
  }

{- FOURMOLU_DISABLE -}

-- | Quaternion binary decoder
getQuaternion :: Get a -> Get (Vector (Quaternion a))
getQuaternion getter = getVector $ do
  v3 <- V3 <$> getter <*> getter <*> getter
  Quaternion
    <$> getter
    <*> pure v3

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
