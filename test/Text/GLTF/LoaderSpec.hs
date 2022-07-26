module Text.GLTF.LoaderSpec (spec) where

import Text.GLTF.Loader

import Lens.Micro
import RIO
import Test.Hspec

spec :: Spec
spec = do
  describe "fromByteString" $ do
    it "Parses embedded gltf content" $ do
      gltfText <- readFileBinary "data/cube.gltf"
      fromByteString gltfText `shouldSatisfy` has _Right

    it "Fails on invalid content" $ do
      fromByteString "Invalid GLTF" `shouldSatisfy` has (_Left . _ReadError)

  describe "fromFile" $ do
    it "Parses embedded gltf file" $ do
      gltf <- fromFile "data/cube.gltf"
      gltf `shouldSatisfy` has _Right

    it "Parses expected nodes" $ do
      gltf <- fromFile "data/cube.gltf"
      let expected = Gltf
            { gltfAsset = Asset
                { version = "2.0",
                  generator = Just $ "Khronos glTF Blender I/O v3.2.40",
                  copyright = Nothing,
                  minVersion = Nothing
                },

              gltfNodes
                = [ Node
                      { rotation = Nothing,
                        scale = Nothing,
                        translation = Nothing,
                        weights = [],
                        name = Just "Cube"
                      }
                  ]
            }
      
      gltf `shouldBe` Right expected
