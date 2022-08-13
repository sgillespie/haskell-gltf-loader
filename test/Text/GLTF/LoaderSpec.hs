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
      res <- fromByteString gltfText
      res `shouldSatisfy` has _Right

    it "Fails on invalid content" $ do
      res <- fromByteString "Invalid GLTF"
      res `shouldSatisfy` has (_Left . _ReadError)

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
                                 meshPrimitiveIndices
                                   = [ 1, 14, 20, 1, 20, 7, 10, 6, 19, 10, 19, 23,
                                       21, 18, 12, 21, 12, 15, 16, 3, 9, 16, 9,
                                       22, 5, 2, 8, 5, 8, 11, 17, 13, 0, 17, 0, 4
                                     ],
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
