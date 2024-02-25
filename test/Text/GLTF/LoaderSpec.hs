module Text.GLTF.LoaderSpec (spec) where

import Text.GLTF.Loader

import Lens.Micro
import Lens.Micro.Platform ()
import Linear (V2 (..), V3 (..), V4 (..))
import RIO
import Test.Hspec

spec :: Spec
spec = do
  describe "fromJsonByteString" $ do
    it "Parses embedded gltf content" $ do
      gltfText <- readFileBinary "data/cube.gltf"
      res <- fromJsonByteString gltfText
      res `shouldSatisfy` has _Right

    it "Fails on invalid content" $ do
      res <- fromJsonByteString "Invalid GLTF"
      res `shouldSatisfy` has (_Left . _ReadError)

  describe "fromJsonFile" $ do
    describe "Gltf embedded" $ gltfFromFileTests "data/cube.gltf"
    describe "Gltf separate" $ gltfFromFileTests "data/cube-separate.gltf"

  describe "fromBinaryBinaryString" $ do
    it "parses glb content" $ do
      glb <- readFileBinary "data/cube.glb"
      res <- fromBinaryByteString glb
      res `shouldSatisfy` has _Right

  describe "fromBinaryFile" $ do
    describe "Glb" $ glbFromFileTests "data/cube.glb"

gltfFromFileTests :: FilePath -> Spec
gltfFromFileTests file = fromFileTests file fromJsonFile

glbFromFileTests :: FilePath -> Spec
glbFromFileTests file = fromFileTests file fromFile
  where
    fromFile path = do
      glb <- fromBinaryFile path
      pure $ over _Right (^. _gltf) glb

fromFileTests
  :: FilePath
  -> (FilePath -> IO (Either Errors Gltf))
  -> Spec
fromFileTests file fromFile = do
  it "Parses embedded gltf file" $ do
    gltf <- fromFile file
    gltf `shouldSatisfy` has _Right

  it "Parses expected nodes" $ do
    gltf <- fromFile file

    let gltf' = gltf ^?! _Right

    gltf' ^? _asset `shouldBe` Just expectedAsset
    gltf' ^?! _images `shouldSatisfy` \images ->
      (images ^? _head . _imageMimeType == Just "image/png")
        && (images ^? _head . _imageName == Just (Just "Untitled"))
    gltf' ^? _materials `shouldBe` Just expectedMaterials
    gltf' ^? _meshes `shouldBe` Just expectedMeshes
    gltf' ^? _nodes `shouldBe` Just expectedNodes
    gltf' ^? _samplers `shouldBe` Just expectedSamplers
    gltf' ^? _scenes `shouldBe` Just expectedScenes
    gltf' ^? _textures `shouldBe` Just expectedTextures

expectedAsset :: Asset
expectedAsset =
  Asset
    { assetVersion = "2.0",
      assetGenerator = Just "Khronos glTF Blender I/O v3.3.27",
      assetCopyright = Nothing,
      assetMinVersion = Nothing
    }

expectedMaterials :: Vector Material
expectedMaterials =
  [ Material
      { materialAlphaCutoff = 0.5,
        materialAlphaMode = Opaque,
        materialDoubleSided = True,
        materialEmissiveFactor = V3 0.0 0.0 0.0,
        materialName = Just "Material",
        materialPbrMetallicRoughness =
          Just
            $ PbrMetallicRoughness
              { pbrBaseColorFactor = V4 1 1 1 1,
                pbrBaseColorTexture =
                  Just
                    $ TextureInfo
                      { textureId = 0,
                        textureTexCoord = 0
                      },
                pbrMetallicFactor = 0.0,
                pbrRoughnessFactor = 0.5
              }
      }
  ]

expectedMeshes :: Vector Mesh
expectedMeshes =
  [ Mesh
      { meshPrimitives =
          [ MeshPrimitive
              { meshPrimitiveMode = Triangles,
                meshPrimitiveIndices =
                  [ 1,
                    14,
                    20,
                    1,
                    20,
                    7,
                    10,
                    6,
                    19,
                    10,
                    19,
                    23,
                    21,
                    18,
                    12,
                    21,
                    12,
                    15,
                    16,
                    3,
                    9,
                    16,
                    9,
                    22,
                    5,
                    2,
                    8,
                    5,
                    8,
                    11,
                    17,
                    13,
                    0,
                    17,
                    0,
                    4
                  ],
                meshPrimitiveMaterial = Just 0,
                meshPrimitivePositions =
                  [ V3 1.0 1.0 (-1.0), -- 1
                    V3 1.0 1.0 (-1.0), -- 2
                    V3 1.0 1.0 (-1.0), -- 3
                    V3 1.0 (-1.0) (-1.0), -- 4
                    V3 1.0 (-1.0) (-1.0), -- 5
                    V3 1.0 (-1.0) (-1.0), -- 6
                    V3 1.0 1.0 1.0, -- 7
                    V3 1.0 1.0 1.0, -- 8
                    V3 1.0 1.0 1.0, -- 9
                    V3 1.0 (-1.0) 1.0, -- 10
                    V3 1.0 (-1.0) 1.0, -- 11
                    V3 1.0 (-1.0) 1.0, -- 12
                    V3 (-1.0) 1.0 (-1.0), -- 13
                    V3 (-1.0) 1.0 (-1.0), -- 14
                    V3 (-1.0) 1.0 (-1.0), -- 15
                    V3 (-1.0) (-1.0) (-1.0), -- 16
                    V3 (-1.0) (-1.0) (-1.0), -- 17
                    V3 (-1.0) (-1.0) (-1.0), -- 18
                    V3 (-1.0) 1.0 1.0, -- 19
                    V3 (-1.0) 1.0 1.0, -- 20
                    V3 (-1.0) 1.0 1.0, -- 21
                    V3 (-1.0) (-1.0) 1.0, -- 22
                    V3 (-1.0) (-1.0) 1.0, -- 23
                    V3 (-1.0) (-1.0) 1.0 -- 24
                  ],
                meshPrimitiveNormals =
                  [ V3 0.0 0.0 (-1.0),
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
                meshPrimitiveTexCoords =
                  [ V2 0.625 0.5,
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
                  ],
                meshPrimitiveColors = []
              }
          ],
        meshWeights = [],
        meshName = Just "Cube"
      }
  ]

expectedNodes :: Vector Node
expectedNodes =
  [ Node
      { nodeChildren = [],
        nodeMeshId = Just 0,
        nodeName = Just "Cube",
        nodeRotation = Nothing,
        nodeScale = Nothing,
        nodeTranslation = Nothing,
        nodeWeights = []
      }
  ]

expectedSamplers :: Vector Sampler
expectedSamplers =
  [ Sampler
      { samplerMagFilter = Just MagLinear,
        samplerMinFilter = Just MinLinearMipmapLinear,
        samplerName = Nothing,
        samplerWrapS = Repeat,
        samplerWrapT = Repeat
      }
  ]

expectedScenes :: Vector Scene
expectedScenes =
  [ Scene
      { sceneName = Just "Scene",
        sceneNodes = [0]
      }
  ]

expectedTextures :: Vector Texture
expectedTextures =
  [ Texture
      { textureName = Nothing,
        textureSamplerId = Just 0,
        textureSourceId = Just 0
      }
  ]
