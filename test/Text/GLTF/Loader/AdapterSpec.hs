module Text.GLTF.Loader.AdapterSpec (spec) where

import Text.GLTF.Loader.Adapter
import Text.GLTF.Loader.Gltf

import Linear (V3(..), V4(..))
import RIO
import Test.Hspec
import qualified Codec.GlTF as GlTF
import qualified Codec.GlTF.Asset as GlTF.Asset
import qualified Codec.GlTF.Mesh as GlTF.Mesh
import qualified Codec.GlTF.Node as GlTF.Node

spec :: Spec
spec = do
  describe "adaptGltf" $ do
    it "Adapts a basic GlTF" $
      adaptGltf codecGltf `shouldBe` loaderGltf

  describe "adaptAsset" $ do
    it "Adapts a basic asset" $ 
      adaptAsset codecAsset `shouldBe` loaderAsset

  describe "adaptNodes" $ do
    it "Adapts a list of nodes" $ do
      let nodes = Just [codecNode, codecNode { GlTF.Node.rotation = Nothing }]
      adaptNodes nodes `shouldBe` [loaderNode, set _nodeRotation Nothing loaderNode]

    it "Adapts empty nodes" $ do
      adaptNodes (Just []) `shouldBe` []
      adaptNodes Nothing `shouldBe` []

  describe "adaptNode" $ do
    it "Adapts a basic node" $ do
      adaptNode codecNode `shouldBe` loaderNode

    it "Adapts empty weights" $ do
      let nodeEmptyWeight = set _nodeWeights [] loaderNode
      adaptNode (codecNode { GlTF.Node.weights = Nothing }) `shouldBe` nodeEmptyWeight
      adaptNode (codecNode { GlTF.Node.weights = Just [] }) `shouldBe` nodeEmptyWeight

codecGltf :: GlTF.GlTF
codecGltf = GlTF.GlTF
  { asset = codecAsset,
    extensionsUsed = Nothing,
    extensionsRequired = Nothing,
    accessors = Nothing,
    animations = Nothing,
    buffers = Nothing,
    bufferViews = Nothing,
    cameras = Nothing,
    images = Nothing,
    materials = Nothing,
    meshes = Nothing,
    nodes = Just [codecNode],
    samplers = Nothing,
    scenes = Nothing,
    skins = Nothing,
    textures = Nothing,
    extensions = Nothing,
    extras = Nothing
  }

loaderGltf :: Gltf
loaderGltf = Gltf
  { gltfAsset = loaderAsset,
    gltfNodes = [loaderNode]
  }

codecAsset :: GlTF.Asset.Asset
codecAsset = GlTF.Asset.Asset
  { version = "version",
    copyright = Just "copyright",
    generator = Just "generator",
    minVersion = Just "minVersion",
    extensions = Nothing,
    extras = Nothing
  }

loaderAsset :: Asset
loaderAsset = Asset
  { assetVersion = "version",
    assetCopyright = Just "copyright",
    assetGenerator = Just "generator",
    assetMinVersion = Just "minVersion"
  }

codecNode :: GlTF.Node.Node
codecNode = GlTF.Node.Node
  { camera = Nothing,
    children = Nothing,
    skin = Nothing,
    matrix = Nothing,
    mesh = Just (GlTF.Mesh.MeshIx 5),
    rotation = Just (1, 2, 3, 4),
    scale = Just (5, 6, 7),
    translation = Just (8, 9, 10),
    weights = Just [11, 12, 13],
    name = Just "node",
    extensions = Nothing,
    extras = Nothing
  }

loaderNode :: Node
loaderNode = Node
  { nodeMeshId = Just 5,
    nodeName = Just "node",
    nodeRotation = Just $ V4 1 2 3 4,
    nodeScale = Just $ V3 5 6 7,
    nodeTranslation = Just $ V3 8 9 10,
    nodeWeights = [11, 12, 13]
  }
