module Text.GLTF.LoaderSpec (spec) where

import Text.GLTF.Loader

import Lens.Micro
import Linear (V2(..), V3(..), V4(..))
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

              gltfMaterials
                = [ Material
                      { materialAlphaCutoff = 0.5,
                        materialAlphaMode = Opaque,
                        materialDoubleSided = True,
                        materialEmissiveFactor = V3 0.0 0.0 0.0,
                        materialName = Just "Material",
                        materialPbrMetallicRoughness = Just $ PbrMetallicRoughness
                          { pbrBaseColorFactor = V4 0.8 0.8 0.8 1.0,
                            pbrMetallicFactor = 0.0,
                            pbrRoughnessFactor = 0.4
                          }
                      }
                  ],
              
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
                                 meshPrimitiveMaterial = Just 0,
                                 meshPrimitivePositions
                                   =  [ V3 1.0 1.0 (-1.0),      -- 1
                                        V3 1.0 1.0 (-1.0),      -- 2
                                        V3 1.0 1.0 (-1.0),      -- 3
                                        V3 1.0 (-1.0) (-1.0),   -- 4
                                        V3 1.0 (-1.0) (-1.0),   -- 5
                                        V3 1.0 (-1.0) (-1.0),   -- 6
                                        V3 1.0 1.0 1.0,         -- 7
                                        V3 1.0 1.0 1.0,         -- 8
                                        V3 1.0 1.0 1.0,         -- 9
                                        V3 1.0 (-1.0) 1.0,      -- 10
                                        V3 1.0 (-1.0) 1.0,      -- 11
                                        V3 1.0 (-1.0) 1.0,      -- 12
                                        V3 (-1.0) 1.0 (-1.0),   -- 13
                                        V3 (-1.0) 1.0 (-1.0),   -- 14
                                        V3 (-1.0) 1.0 (-1.0),   -- 15
                                        V3 (-1.0) (-1.0) (-1.0),-- 16
                                        V3 (-1.0) (-1.0) (-1.0),-- 17
                                        V3 (-1.0) (-1.0) (-1.0),-- 18
                                        V3 (-1.0) 1.0 1.0,      -- 19
                                        V3 (-1.0) 1.0 1.0,      -- 20
                                        V3 (-1.0) 1.0 1.0,      -- 21
                                        V3 (-1.0) (-1.0) 1.0,   -- 22
                                        V3 (-1.0) (-1.0) 1.0,   -- 23
                                        V3 (-1.0) (-1.0) 1.0    -- 24
                                      ],
                                 meshPrimitiveNormals
                                   = [ V3 0.0 0.0 (-1.0),
                                       V3 0.0 1.0 (-0.0),
                                       V3 1.0 0.0 (-0.0),
                                       V3 0.0 (-1.0) (-0.0),
                                       V3 0.0 0.0 (-1.0),
                                       V3 1.0 0.0 (-0.0),
                                       V3 0.0 0.0 1.0,
                                       V3 0.0 1.0 (-0.0),
                                       V3 1.0 0.0 (-0.0),
                                       V3 0.0 (-1.0) (-0.0),
                                       V3 0.0 0.0 1.0,
                                       V3 1.0 0.0 (-0.0),
                                       V3 (-1.0) 0.0 (-0.0),
                                       V3 0.0 0.0 (-1.0),
                                       V3 0.0 1.0 (-0.0),
                                       V3 (-1.0) 0.0 (-0.0),
                                       V3 0.0 (-1.0) (-0.0),
                                       V3 0.0 0.0 (-1.0),
                                       V3 (-1.0) 0.0 (-0.0),
                                       V3 0.0 0.0 1.0,
                                       V3 0.0 1.0 (-0.0),
                                       V3 (-1.0) 0.0 (-0.0),
                                       V3 0.0 (-1.0) (-0.0),
                                       V3 0.0 0.0 1.0
                                     ],
                                 meshPrimitiveTexCoords
                                   = [ V2 0.625 0.5,
                                       V2 0.625 0.5,
                                       V2 0.625 0.5,
                                       V2 0.375 0.5,
                                       V2 0.375 0.5,
                                       V2 0.375 0.5,
                                       V2 0.625 0.25,
                                       V2 0.625 0.25,
                                       V2 0.625 0.25,
                                       V2 0.375 0.25,
                                       V2 0.375 0.25,
                                       V2 0.375 0.25,
                                       V2 0.625 0.75,
                                       V2 0.625 0.75,
                                       V2 0.875 0.5,
                                       V2 0.375 0.75,
                                       V2 0.125 0.5,
                                       V2 0.375 0.75,
                                       V2 0.625 1.0,
                                       V2 0.625 0.0,
                                       V2 0.875 0.25,
                                       V2 0.375 1.0,
                                       V2 0.125 0.25,
                                       V2 0.375 0.0
                                     ]
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
