module Text.GLTF.Loader.AdapterSpec (spec) where
import Text.GLTF.Loader.Adapter
import Text.GLTF.Loader.BufferAccessor
import Text.GLTF.Loader.Gltf
import Text.GLTF.Loader.MonadAdapter
import Text.GLTF.Loader.Test.MkGltf

import Linear
import RIO
import Test.Hspec
import qualified Codec.GlTF.BufferView as BufferView
import qualified Codec.GlTF.Image as Image
import qualified Codec.GlTF.Material as Material
import qualified Codec.GlTF.Mesh as Mesh
import qualified Codec.GlTF.Node as Node
import qualified Codec.GlTF.URI as URI

spec :: Spec
spec = do
  let codecGltf = mkCodecGltf
      codecMeshPrimitive = mkCodecMeshPrimitive

  describe "runAdapter" $ do
    it "Runs a basic GlTF adapter" $ do
      buffers' <- buffers
      images' <- images
      
      runAdapter codecGltf buffers' images' `shouldBe` loaderGltf

  describe "adaptGltf" $ do
    it "Adapts a basic GlTF" $ do
      env' <- env
      runReader adaptGltf env' `shouldBe` loaderGltf

  describe "adaptAsset" $ do
    let codecAsset = mkCodecAsset
    
    it "Adapts a basic asset" $ 
      adaptAsset codecAsset `shouldBe` loaderAsset

  describe "adaptMeshes" $ do
    let codecMesh = mkCodecMesh
        codecMesh' = mkCodecMesh { Mesh.weights = Just [3.1] }
    
    it "Adapts a list of nodes" $ do
      env' <- env
      
      let meshes = Just [codecMesh, codecMesh']
          adaptedMeshes = [loaderMesh, set _meshWeights [3.1] loaderMesh]

      runReader (adaptMeshes meshes) env' `shouldBe` adaptedMeshes

    it "Adapts empty meshes" $ do
      env' <- env
      
      runReader (adaptMeshes (Just [])) env' `shouldBe` []
      runReader (adaptMeshes Nothing) env' `shouldBe` []

  describe "adaptMaterials" $ do
    let materials = Just [mkCodecMaterial]

    it "Adapts a list of materials" $ do
      adaptMaterials Nothing `shouldBe` []
      adaptMaterials materials `shouldBe` [loaderMaterial]

    it "Ignores PBR metallic roughness when not specified" $ do
      let materials' = Just
            [mkCodecMaterial { Material.pbrMetallicRoughness = Nothing }]
          adaptedMaterial = set _materialPbrMetallicRoughness Nothing loaderMaterial

      adaptMaterials materials' `shouldBe` [adaptedMaterial]

  describe "adaptNodes" $ do
    let codecNode = mkCodecNode
        codecNode' = codecNode { Node.rotation = Nothing }
    
    it "Adapts a list of nodes" $ do
      let nodes = Just [codecNode, codecNode']
      adaptNodes nodes `shouldBe` [loaderNode, set _nodeRotation Nothing loaderNode]

    it "Adapts empty nodes" $ do
      adaptNodes (Just []) `shouldBe` []
      adaptNodes Nothing `shouldBe` []

  describe "adaptImage" $ do
    let codecImage = Image.Image
            { uri = Nothing,
              mimeType = Just "text/jpg",
              bufferView = Just $ BufferView.BufferViewIx 4,
              name = Just "Image",
              extensions = Nothing,
              extras = Nothing
              
            }
    
    it "Adapts a BufferView image" $ do
      env' <- env
      let image = ImageBufferView (BufferView.BufferViewIx 4)
      
      runReader (adaptImage image codecImage) env' `shouldBe`
        Image
          { imageData = Just "imageData",
            imageMimeType = "text/jpg",
            imageName = Just "Image"
          }

    it "Adapts a URI image" $ do
      env' <- env
      
      let image = ImageData "imageData"
          codecImage' = codecImage
            { Image.uri = Just $ URI.URI "",
              Image.bufferView = Nothing
            }

      runReader (adaptImage image codecImage') env' `shouldBe`
        Image
          { imageData = Just "imageData",
            imageMimeType = "text/jpg",
            imageName = Just "Image"
          }
      

  describe "adaptMesh" $ do
    let codecMesh = mkCodecMesh
        codecMesh' = mkCodecMesh { Mesh.weights = Nothing }
        codecMesh'' = mkCodecMesh { Mesh.weights = Just [] }
    
    it "Adapts a basic mesh" $ do
      env' <- env
      runReader (adaptMesh codecMesh) env' `shouldBe` loaderMesh

    it "Adapts empty weights" $ do
      env' <- env
      let meshEmptyWeight = set _meshWeights [] loaderMesh
      
      runReader (adaptMesh codecMesh') env' `shouldBe` meshEmptyWeight
      runReader (adaptMesh codecMesh'') env' `shouldBe` meshEmptyWeight
  
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

  describe "adaptAlphaMode" $ do
    it "Adapts all expected modes" $ do
      adaptAlphaMode Material.BLEND `shouldBe` Blend
      adaptAlphaMode Material.MASK `shouldBe` Mask
      adaptAlphaMode Material.OPAQUE `shouldBe` Opaque
      evaluate (adaptAlphaMode $ Material.MaterialAlphaMode "???")
        `shouldThrow` anyErrorCall

  describe "adaptMeshPrimitives" $ do
    let codecMeshPrimitive' = mkCodecMeshPrimitive
          { Mesh.mode = Mesh.MeshPrimitiveMode 0 }
  
    it "adapts a list of primitives" $ do
      env' <- env
      let primitives = [codecMeshPrimitive, codecMeshPrimitive']
          expectedResult
            = [ loaderMeshPrimitive,
                set _meshPrimitiveMode Points loaderMeshPrimitive
              ]
      
      runReader (adaptMeshPrimitives primitives) env' `shouldBe` expectedResult

  describe "adaptMeshPrimitive" $ do
    it "adapts a basic primitive" $ do
      env' <- env
      runReader (adaptMeshPrimitive codecMeshPrimitive) env' `shouldBe` loaderMeshPrimitive

    it "ignores indices when unspecified" $ do
      env' <- env
      
      let codecMeshPrimitive' = mkCodecMeshPrimitive
            { Mesh.indices = Nothing }
          loaderMeshPrimitive' = loaderMeshPrimitive & _meshPrimitiveIndices .~ []

      runReader (adaptMeshPrimitive codecMeshPrimitive') env' `shouldBe` loaderMeshPrimitive'

    it "ignores material when unspecified" $ do
      env' <- env

      let codecMeshPrimitive' = mkCodecMeshPrimitive
            { Mesh.material = Nothing }
          loaderMeshPrimitive' = loaderMeshPrimitive & _meshPrimitiveMaterial .~ Nothing

      runReader (adaptMeshPrimitive codecMeshPrimitive') env'
        `shouldBe` loaderMeshPrimitive'
      
    
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

images :: MonadUnliftIO io => io (Vector GltfImageData)
images = loadImages mkCodecGltf

env :: MonadUnliftIO io => io AdaptEnv
env = AdaptEnv mkCodecGltf <$> buffers <*> images

loaderGltf :: Gltf
loaderGltf = Gltf
  { gltfAsset = loaderAsset,
    gltfImages = [loaderImage],
    gltfMaterials = [loaderMaterial],
    gltfMeshes = [loaderMesh],
    gltfNodes = [loaderNode],
    gltfSamplers = [loaderSampler],
    gltfTextures = []
  }

loaderAsset :: Asset
loaderAsset = Asset
  { assetVersion = "version",
    assetCopyright = Just "copyright",
    assetGenerator = Just "generator",
    assetMinVersion = Just "minVersion"
  }

loaderImage :: Image
loaderImage = Image
  { imageData = Just "imagePayload",
    imageMimeType = "image/png",
    imageName = Just "Image"
  }

loaderMaterial :: Material
loaderMaterial = Material
  { materialAlphaCutoff = 1.0,
    materialAlphaMode = Opaque,
    materialDoubleSided = True,
    materialEmissiveFactor = V3 1.0 2.0 3.0,
    materialName = Just "Material",
    materialPbrMetallicRoughness = Just loaderPbrMetallicRoughness
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

loaderSampler :: Sampler
loaderSampler = Sampler
  {  samplerMagFilter = Just MagLinear,
     samplerMinFilter = Just MinLinear,
     samplerName = Just "Sampler",
     samplerWrapS = ClampToEdge,
     samplerWrapT = Repeat
  }

loaderPbrMetallicRoughness :: PbrMetallicRoughness
loaderPbrMetallicRoughness = PbrMetallicRoughness
  { pbrBaseColorFactor = V4 1.0 2.0 3.0 4.0,
    pbrBaseColorTexture = Just loaderBaseColorTexture,
    pbrMetallicFactor = 1.0,
    pbrRoughnessFactor = 2.0
  }

loaderMeshPrimitive :: MeshPrimitive
loaderMeshPrimitive = MeshPrimitive
  { meshPrimitiveIndices = [1..4],
    meshPrimitiveMaterial = Just 1,
    meshPrimitiveMode = Triangles,
    meshPrimitiveNormals = fmap (\x -> V3 x x x) [5..8],
    meshPrimitivePositions = fmap (\x -> V3 x x x) [1..4],
    meshPrimitiveTexCoords = fmap (\x -> V2 x x) [9..12]
  }

loaderBaseColorTexture :: TextureInfo
loaderBaseColorTexture = TextureInfo
  { textureId = 15,
    textureTexCoord = 10
  }
