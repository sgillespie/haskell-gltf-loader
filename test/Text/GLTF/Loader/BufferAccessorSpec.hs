module Text.GLTF.Loader.BufferAccessorSpec (spec) where

import Text.GLTF.Loader.BufferAccessor
import Text.GLTF.Loader.Test.MkGltf

import Linear (V3(..))
import Data.Binary.Builder
import Data.Binary.Get (runGet)
import Data.Binary.Put (putFloatle, runPut)
import Data.ByteString.Lazy (fromStrict, toStrict)
import RIO
import RIO.Vector.Partial ((!))
import Test.Hspec
import qualified Codec.GlTF as GlTF
import qualified Codec.GlTF.Accessor as Accessor
import qualified Codec.GlTF.Buffer as Buffer
import qualified Codec.GlTF.BufferView as BufferView
import qualified Codec.GlTF.URI as URI

spec :: Spec
spec = do
  let gltf = mkCodecGltf
  
  describe "loadBuffers" $ do
    it "Reads buffers from GlTF" $ do
      buffers <- loadBuffers gltf
      
      let (GltfBuffer buffer') = buffers ! 0
          values = runGet (getScalar (fromIntegral <$> getUnsignedShort)) . fromStrict $ buffer'
      
      values `shouldBe` ([1..4] :: Vector Integer)

    it "Handles malformed URI" $ do
      let gltf' = gltf
            { GlTF.buffers = Just
                [ mkCodecBufferIndices { Buffer.uri = Just $ URI.URI "uh oh!" } ]
            }

      loadBuffers gltf' `shouldThrow` anyException

    it "Handles no buffer" $ do
      let gltf' = gltf { GlTF.buffers = Nothing }
      buffers <- loadBuffers gltf'

      buffers `shouldBe` []
    
  
  describe "vertexIndices" $ do
    it "Reads basic values from buffer" $ do
      vertexIndices gltf buffers' accessorIdIndices `shouldBe` [1, 2, 3, 4]

    it "Returns empty when accessor not defined" $ do
      let gltf' = gltf { GlTF.accessors = Nothing } 
      vertexIndices gltf' buffers' accessorIdIndices `shouldBe` []

    it "Returns empty when buffer not found" $ do
      let bufferView = mkCodecBufferViewIndices { BufferView.buffer = Buffer.BufferIx 99 }
          gltf' = gltf { GlTF.bufferViews = Just [bufferView] }
      vertexIndices gltf' buffers' accessorIdIndices `shouldBe` []

  describe "vertexPositions" $ do
    it "Reads basic values from buffer" $ do
      vertexPositions gltf buffers' accessorIdPositions `shouldBe`
        [ V3 1 1 1,
          V3 2 2 2,
          V3 3 3 3,
          V3 4 4 4
        ]

    it "Returns empty when accessor not defined" $ do
      let gltf' = gltf { GlTF.accessors = Nothing } 
      vertexPositions gltf' buffers' accessorIdPositions `shouldBe` []

    it "Returns empty when buffer not found" $ do
      let bufferView = mkCodecBufferViewIndices { BufferView.buffer = Buffer.BufferIx 99 }
          gltf' = gltf { GlTF.bufferViews = Just [bufferView] }
      vertexPositions gltf' buffers' accessorIdPositions `shouldBe` []

buffers' :: Vector GltfBuffer
buffers' = [bufferIndices, bufferPositions]

accessorIdIndices :: Accessor.AccessorIx
accessorIdIndices = Accessor.AccessorIx 0

accessorIdPositions :: Accessor.AccessorIx
accessorIdPositions = Accessor.AccessorIx 1

bufferIndices :: GltfBuffer
bufferIndices = GltfBuffer . toStrict . toLazyByteString $ putIndices
  where putIndices = foldr ((<>) . putWord16le) empty ([1, 2, 3, 4] :: [Word16])

bufferPositions :: GltfBuffer
bufferPositions = GltfBuffer . toStrict . runPut $ putPositions
  where putPositions = mapM_ (replicateM_ 3 . putFloatle) ([1..4] :: [Float])
