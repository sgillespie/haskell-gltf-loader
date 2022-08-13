module Text.GLTF.Loader.BufferAccessorSpec (spec) where

import Text.GLTF.Loader.BufferAccessor
import Text.GLTF.Loader.Test.MkGltf

import Data.Binary.Builder
import Data.Binary.Get (runGet)
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
          values = runGet getIndices . fromStrict $ buffer'
      
      values `shouldBe` [1..4]

    it "Handles malformed URI" $ do
      let gltf' = gltf
            { GlTF.buffers = Just
                [ mkCodecBuffer { Buffer.uri = Just $ URI.URI "uh oh!" } ]
            }

      loadBuffers gltf' `shouldThrow` anyException

    it "Handles no buffer" $ do
      let gltf' = gltf { GlTF.buffers = Nothing }
      buffers <- loadBuffers gltf'

      buffers `shouldBe` []
    
  
  describe "vertexIndices" $ do
    it "Reads basic values from buffer" $ do
      vertexIndices gltf buffers' accessorId `shouldBe` [1, 2, 3, 4]

    it "Returns empty when accessor not defined" $ do
      let gltf' = gltf { GlTF.accessors = Nothing } 
      vertexIndices gltf' buffers' accessorId `shouldBe` []

    it "Returns empty when buffer not found" $ do
      let bufferView = mkCodecBufferView { BufferView.buffer = Buffer.BufferIx 1 }
          gltf' = gltf { GlTF.bufferViews = Just [bufferView] }
      vertexIndices gltf' buffers' accessorId `shouldBe` []

buffers' :: Vector GltfBuffer
buffers' = [buffer]

accessorId :: Accessor.AccessorIx
accessorId = Accessor.AccessorIx 0

buffer :: GltfBuffer
buffer = GltfBuffer . toStrict . toLazyByteString $ putIndices
  where putIndices = foldr ((<>) . putWord16le) empty ([1, 2, 3, 4] :: [Word16])
