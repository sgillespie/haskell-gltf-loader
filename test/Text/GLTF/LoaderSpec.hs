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
                { assetVersion = "2.0",
                  assetGenerator = Just "Khronos glTF Blender I/O v3.2.40",
                  assetCopyright = Nothing,
                  assetMinVersion = Nothing
                },

              gltfMeshes
                = [ Mesh
                     { meshPrimitives
                         = [ MeshPrimitive
                               { meshPrimitiveMode = Triangles,
                                 vertexIndices = [],
                                 vertexPositions = [],
                                 vertexNormals = []
                               }
                           ],
                       meshWeights = [],
                       meshName = Just "Cube"
                     }
                  ],

              gltfNodes
                = [ Node
                      { nodeMeshId = Just 0,
                        nodeName = Just "Cube",
                        nodeRotation = Nothing,
                        nodeScale = Nothing,
                        nodeTranslation = Nothing,
                        nodeWeights = []
                      }
                  ]
            }
      
      gltf `shouldBe` Right expected
