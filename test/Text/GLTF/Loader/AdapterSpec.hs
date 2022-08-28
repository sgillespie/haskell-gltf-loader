module Text.GLTF.Loader.AdapterSpec (spec) where

import Text.GLTF.Loader.Adapter
import Text.GLTF.Loader.BufferAccessor
import Text.GLTF.Loader.Gltf
import Text.GLTF.Loader.Test.MkGltf

import Linear
import RIO
import Test.Hspec
import qualified Codec.GlTF.Mesh as Mesh
import qualified Codec.GlTF.Node as Node

spec :: Spec
spec = do
  let codecGltf = mkCodecGltf
      codecMeshPrimitive = mkCodecMeshPrimitive

  describe "adaptGltf" $ do
    it "Adapts a basic GlTF" $ do
      buffers' <- buffers
      adaptGltf codecGltf buffers' `shouldBe` loaderGltf

  describe "adaptAsset" $ do
    let codecAsset = mkCodecAsset
    
    it "Adapts a basic asset" $ 
      adaptAsset codecAsset `shouldBe` loaderAsset

  describe "adaptMeshes" $ do
    let codecMesh = mkCodecMesh
        codecMesh' = mkCodecMesh { Mesh.weights = Just [3.1] }
    
    it "Adapts a list of nodes" $ do
      buffers' <- buffers
      
      let meshes = Just [codecMesh, codecMesh']
          adaptedMeshes = [loaderMesh, set _meshWeights [3.1] loaderMesh]

      adaptMeshes codecGltf buffers' meshes `shouldBe` adaptedMeshes

    it "Adapts empty meshes" $ do
      buffers' <- buffers
      
      adaptMeshes codecGltf buffers' (Just []) `shouldBe` []
      adaptMeshes codecGltf buffers' Nothing `shouldBe` []

  describe "adaptNodes" $ do
    let codecNode = mkCodecNode
        codecNode' = codecNode { Node.rotation = Nothing }
    
    it "Adapts a list of nodes" $ do
      let nodes = Just [codecNode, codecNode']
      adaptNodes nodes `shouldBe` [loaderNode, set _nodeRotation Nothing loaderNode]

    it "Adapts empty nodes" $ do
      adaptNodes (Just []) `shouldBe` []
      adaptNodes Nothing `shouldBe` []

  describe "adaptMesh" $ do
    let codecMesh = mkCodecMesh
        codecMesh' = mkCodecMesh { Mesh.weights = Nothing }
        codecMesh'' = mkCodecMesh { Mesh.weights = Just [] }
    
    it "Adapts a basic mesh" $ do
      buffers' <- buffers
      adaptMesh codecGltf buffers' codecMesh `shouldBe` loaderMesh

    it "Adapts empty weights" $ do
      buffers' <- buffers
      let meshEmptyWeight = set _meshWeights [] loaderMesh
      
      adaptMesh codecGltf buffers' codecMesh' `shouldBe` meshEmptyWeight
      adaptMesh codecGltf buffers' codecMesh'' `shouldBe` meshEmptyWeight
  
  describe "adaptNode" $ do
    let codecNode = mkCodecNode
        codecNode' = mkCodecNode { Node.weights = Nothing }
        codecNode'' = mkCodecNode { Node.weights = Just [] }
        
    it "Adapts a basic node" $ do
      adaptNode codecNode `shouldBe` loaderNode

    it "Adapts empty weights" $ do
      let nodeEmptyWeight = set _nodeWeights [] loaderNode
      adaptNode codecNode' `shouldBe` nodeEmptyWeight
      adaptNode codecNode'' `shouldBe` nodeEmptyWeight

  describe "adaptMeshPrimitives" $ do
    let codecMeshPrimitive' = mkCodecMeshPrimitive
          { Mesh.mode = Mesh.MeshPrimitiveMode 0 }
  
    it "adapts a list of primitives" $ do
      buffers' <- buffers
      let primitives = [codecMeshPrimitive, codecMeshPrimitive']
          expectedResult
            = [ loaderMeshPrimitive,
                set _meshPrimitiveMode Points loaderMeshPrimitive
              ]
      
      adaptMeshPrimitives codecGltf buffers' primitives `shouldBe` expectedResult

  describe "adaptMeshPrimitive" $ do
    it "adapts a basic primitive" $ do
      buffers' <- buffers

      let codecMeshPrimitive' = mkCodecMeshPrimitive
            { Mesh.indices = Nothing }
          loaderMeshPrimitive' = set _meshPrimitiveIndices [] loaderMeshPrimitive
      
      adaptMeshPrimitive codecGltf buffers' codecMeshPrimitive `shouldBe` loaderMeshPrimitive
      adaptMeshPrimitive codecGltf buffers' codecMeshPrimitive' `shouldBe` loaderMeshPrimitive'
    
  describe "adaptMeshPrimitiveMode" $
    it "Adapts all expected modes" $ do
      adaptMeshPrimitiveMode Mesh.POINTS `shouldBe` Points
      adaptMeshPrimitiveMode Mesh.LINES `shouldBe` Lines
      adaptMeshPrimitiveMode Mesh.LINE_LOOP `shouldBe` LineLoop
      adaptMeshPrimitiveMode Mesh.LINE_STRIP `shouldBe` LineStrip
      adaptMeshPrimitiveMode Mesh.TRIANGLES `shouldBe` Triangles
      adaptMeshPrimitiveMode Mesh.TRIANGLE_STRIP `shouldBe` TriangleStrip
      adaptMeshPrimitiveMode Mesh.TRIANGLE_FAN `shouldBe` TriangleFan
      evaluate (adaptMeshPrimitiveMode $ Mesh.MeshPrimitiveMode 7)
        `shouldThrow` anyErrorCall

buffers :: MonadUnliftIO io => io (Vector GltfBuffer)
buffers = loadBuffers mkCodecGltf

loaderGltf :: Gltf
loaderGltf = Gltf
  { gltfAsset = loaderAsset,
    gltfMeshes = [loaderMesh],
    gltfNodes = [loaderNode]
  }

loaderAsset :: Asset
loaderAsset = Asset
  { assetVersion = "version",
    assetCopyright = Just "copyright",
    assetGenerator = Just "generator",
    assetMinVersion = Just "minVersion"
  }

loaderMesh :: Mesh
loaderMesh = Mesh
  { meshPrimitives = [loaderMeshPrimitive],
    meshWeights = [1.2],
    meshName = Just "mesh"
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

loaderMeshPrimitive :: MeshPrimitive
loaderMeshPrimitive = MeshPrimitive
  { meshPrimitiveMode = Triangles,
    meshPrimitiveIndices = [1..4],
    meshPrimitivePositions = fmap (\x -> V3 x x x) [1..4],
    meshPrimitiveNormals = fmap (\x -> V3 x x x) [5..8],
    meshPrimitiveTexCoords = fmap (\x -> V2 x x) [9..12]
  }
