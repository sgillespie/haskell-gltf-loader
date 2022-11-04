-- |Transform a `Codec.GlTF.GlTF` to `Text.GLTF.Loader.Gltf.Gltf`
module Text.GLTF.Loader.Adapter
  ( attributePosition,
    attributeNormal,
    attributeTexCoord,
    runAdapter,
    adaptGltf,
    adaptAsset,
    adaptImages,
    adaptMaterials,
    adaptMeshes,
    adaptNodes,
    adaptImage,
    adaptMaterial,
    adaptMesh,
    adaptNode,
    adaptAlphaMode,
    adaptPbrMetallicRoughness,
    adaptMeshPrimitives,
    adaptMeshPrimitive,
    adaptMeshPrimitiveMode
  ) where

import Text.GLTF.Loader.BufferAccessor
import Text.GLTF.Loader.Gltf
import Text.GLTF.Loader.MonadAdapter

import Linear (V3(..), V4(..))
import RIO
import RIO.Partial (toEnum)
import RIO.Vector.Partial ((!))
import qualified Codec.GlTF as GlTF
import qualified Codec.GlTF.Asset as Asset
import qualified Codec.GlTF.Image as Image
import qualified Codec.GlTF.Material as Material
import qualified Codec.GlTF.PbrMetallicRoughness as PbrMetallicRoughness
import qualified Codec.GlTF.Mesh as Mesh
import qualified Codec.GlTF.Node as Node
import qualified Codec.GlTF.Sampler as Sampler
import qualified Codec.GlTF.TextureInfo as TextureInfo
import qualified Data.HashMap.Strict as HashMap
import qualified RIO.Vector as V

attributePosition :: Text
attributePosition = "POSITION"

attributeNormal :: Text
attributeNormal = "NORMAL"

attributeTexCoord :: Text
attributeTexCoord = "TEXCOORD_0"

runAdapter
  :: GlTF.GlTF
  -> Vector GltfBuffer
  -> Vector GltfImageData
  -> Gltf
runAdapter gltf buffers images = runReader adaptGltf env
  where env = AdaptEnv gltf buffers images

adaptGltf :: Adapter Gltf
adaptGltf = do
  GlTF.GlTF{..} <- getGltf

  gltfImages <- adaptImages images
  gltfMeshes <- adaptMeshes meshes
  
  return $ Gltf
    { gltfAsset = adaptAsset asset,
      gltfImages = gltfImages,
      gltfMaterials = adaptMaterials materials,
      gltfMeshes = gltfMeshes,
      gltfNodes = adaptNodes nodes,
      gltfSamplers = adaptSamplers samplers,
      gltfTextures = []
    }

adaptAsset :: Asset.Asset -> Asset
adaptAsset Asset.Asset{..} = Asset
  { assetVersion = version,
    assetCopyright = copyright,
    assetGenerator = generator,
    assetMinVersion = minVersion
  }

adaptImages :: Maybe (Vector Image.Image) -> Adapter (Vector Image)
adaptImages codecImages = do
  imageData <- getImages

  let images' = fromMaybe mempty codecImages
      iforM = flip V.imapM

  iforM images' $ \imgId img ->
    adaptImage (imageData ! imgId) img

adaptMaterials :: Maybe (Vector Material.Material) -> Vector Material
adaptMaterials = maybe mempty (fmap adaptMaterial)

adaptMeshes :: Maybe (Vector Mesh.Mesh) -> Adapter (Vector Mesh)
adaptMeshes = maybe (return mempty) (V.mapM adaptMesh)

adaptNodes :: Maybe (Vector Node.Node) -> Vector Node
adaptNodes = maybe mempty (fmap adaptNode)

adaptSamplers :: Maybe (Vector Sampler.Sampler) -> Vector Sampler
adaptSamplers = maybe mempty (fmap adaptSampler)

adaptImage
  :: GltfImageData
  -> Image.Image
  -> Adapter Image
adaptImage (ImageData payload) Image.Image{..}
  = return Image
      { imageData = Just payload,
        imageMimeType = mimeType',
        imageName = name
      }
  where mimeType' = fromMaybe undefined mimeType

adaptImage (ImageBufferView id') Image.Image{..} = do
  gltf <- getGltf
  buffers' <- getBuffers
  
  let mimeType' = fromMaybe undefined mimeType
      payload = imageDataRaw gltf buffers' id'
  
  return Image
      { imageData = payload,
        imageMimeType = mimeType',
        imageName = name
      }

adaptImage _ _ = undefined

adaptMaterial :: Material.Material -> Material
adaptMaterial Material.Material{..} = Material
  { materialAlphaCutoff = alphaCutoff,
    materialAlphaMode = adaptAlphaMode alphaMode,
    materialDoubleSided = doubleSided,
    materialEmissiveFactor = toV3 emissiveFactor,
    materialName = name,
    materialPbrMetallicRoughness = adaptPbrMetallicRoughness <$> pbrMetallicRoughness
  }

adaptMesh :: Mesh.Mesh -> Adapter Mesh
adaptMesh Mesh.Mesh{..} = do
  primitives' <- adaptMeshPrimitives primitives
  
  return $ Mesh
    { meshPrimitives = primitives',
      meshWeights = fromMaybe mempty weights,
      meshName = name
    }

adaptNode :: Node.Node -> Node
adaptNode Node.Node{..} = Node
  { nodeMeshId = Mesh.unMeshIx <$> mesh,
    nodeName = name,
    nodeRotation = toV4 <$> rotation,
    nodeScale = toV3 <$> scale,
    nodeTranslation = toV3 <$> translation,
    nodeWeights = maybe [] toList weights
  }

adaptSampler :: Sampler.Sampler -> Sampler
adaptSampler Sampler.Sampler{..} = Sampler
  { samplerMagFilter = adaptMagFilter <$> magFilter,
    samplerMinFilter = adaptMinFilter <$> minFilter,
    samplerName = name,
    samplerWrapS = adaptSamplerWrap wrapS,
    samplerWrapT = adaptSamplerWrap wrapT
  }

adaptAlphaMode :: Material.MaterialAlphaMode -> MaterialAlphaMode
adaptAlphaMode Material.BLEND = Blend
adaptAlphaMode Material.MASK = Mask
adaptAlphaMode Material.OPAQUE = Opaque
adaptAlphaMode (Material.MaterialAlphaMode alphaMode)
  = error $ "Invalid MaterialAlphaMode: " <> show alphaMode

adaptPbrMetallicRoughness
  :: PbrMetallicRoughness.PbrMetallicRoughness
  -> PbrMetallicRoughness
adaptPbrMetallicRoughness PbrMetallicRoughness.PbrMetallicRoughness{..}
  = PbrMetallicRoughness
    { pbrBaseColorFactor = toV4 baseColorFactor,
      pbrBaseColorTexture = adaptTextureInfo <$> baseColorTexture,
      pbrMetallicFactor = metallicFactor,
      pbrRoughnessFactor = roughnessFactor
    }

adaptMeshPrimitives :: Vector Mesh.MeshPrimitive -> Adapter (Vector MeshPrimitive)
adaptMeshPrimitives = V.mapM adaptMeshPrimitive

adaptMagFilter :: Sampler.SamplerMagFilter -> MagFilter
adaptMagFilter Sampler.MAG_LINEAR = MagLinear
adaptMagFilter Sampler.MAG_NEAREST = MagNearest
adaptMagFilter mode = error $ "Invalid MagFilter: " <> show mode

adaptMinFilter :: Sampler.SamplerMinFilter -> MinFilter
adaptMinFilter Sampler.MIN_NEAREST = MinNearest
adaptMinFilter Sampler.MIN_LINEAR = MinLinear
adaptMinFilter Sampler.MIN_NEAREST_MIPMAP_NEAREST = MinNearestMipmapNearest
adaptMinFilter Sampler.MIN_NEAREST_MIPMAP_LINEAR = MinNearestMipmapLinear
adaptMinFilter Sampler.MIN_LINEAR_MIPMAP_NEAREST = MinLinearMipmapNearest
adaptMinFilter Sampler.MIN_LINEAR_MIPMAP_LINEAR = MinLinearMipmapLinear
adaptMinFilter mode = error $ "Invalid MinFilter: " <> show mode

adaptSamplerWrap :: Sampler.SamplerWrap -> SamplerWrap
adaptSamplerWrap Sampler.CLAMP_TO_EDGE = ClampToEdge
adaptSamplerWrap Sampler.MIRRORED_REPEAT = MirroredRepeat
adaptSamplerWrap Sampler.REPEAT = Repeat
adaptSamplerWrap mode = error $ "Invalid SamplerWrap: " <> show mode

adaptTextureInfo :: TextureInfo.TextureInfo a -> TextureInfo
adaptTextureInfo TextureInfo.TextureInfo{..} = TextureInfo
  { textureId = index,
    textureTexCoord = texCoord
  }

adaptMeshPrimitive :: Mesh.MeshPrimitive -> Adapter MeshPrimitive
adaptMeshPrimitive Mesh.MeshPrimitive{..} = do
  gltf <- getGltf
  buffers' <- getBuffers
  
  return $ MeshPrimitive
    { meshPrimitiveIndices = maybe mempty (vertexIndices gltf buffers') indices,
      meshPrimitiveMaterial = Material.unMaterialIx <$> material,
      meshPrimitiveMode = adaptMeshPrimitiveMode mode,
      meshPrimitiveNormals = maybe mempty (vertexNormals gltf buffers') normals,
      meshPrimitivePositions = maybe mempty (vertexPositions gltf buffers') positions,
      meshPrimitiveTexCoords = maybe mempty (vertexTexCoords gltf buffers') texCoords
    }
    where positions = attributes HashMap.!? attributePosition
          normals = attributes HashMap.!? attributeNormal
          texCoords = attributes HashMap.!? attributeTexCoord
          

adaptMeshPrimitiveMode :: Mesh.MeshPrimitiveMode -> MeshPrimitiveMode
adaptMeshPrimitiveMode = toEnum . Mesh.unMeshPrimitiveMode

toV3 :: (a, a, a) -> V3 a
toV3 (x, y, z) = V3 x y z

toV4 :: (a, a, a, a) -> V4 a
toV4 (w, x, y, z) = V4 w x y z
