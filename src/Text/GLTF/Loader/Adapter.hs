-- |Transform a `Codec.GlTF.GlTF` to `Text.GLTF.Loader.Gltf.Gltf`
module Text.GLTF.Loader.Adapter
  ( attributePosition,
    attributeNormal,
    attributeTexCoord,
    adaptGltf,
    adaptAsset,
    adaptMaterials,
    adaptMeshes,
    adaptNodes,
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

import Linear (V3(..), V4(..))
import RIO
import RIO.Partial (toEnum)
import qualified Codec.GlTF as GlTF
import qualified Codec.GlTF.Asset as GlTF.Asset
import qualified Codec.GlTF.Material as GlTF.Material
import qualified Codec.GlTF.PbrMetallicRoughness as GlTF.PbrMetallicRoughness
import qualified Codec.GlTF.Mesh as GlTF.Mesh
import qualified Codec.GlTF.Node as GlTF.Node
import qualified Codec.GlTF.TextureInfo as GlTF.TextureInfo
import qualified Data.HashMap.Strict as HashMap

attributePosition :: Text
attributePosition = "POSITION"

attributeNormal :: Text
attributeNormal = "NORMAL"

attributeTexCoord :: Text
attributeTexCoord = "TEXCOORD_0"

adaptGltf :: GlTF.GlTF -> Vector GltfBuffer -> Gltf
adaptGltf gltf@GlTF.GlTF{..} buffers' = Gltf
    { gltfAsset = adaptAsset asset,
      gltfImages = [],
      gltfMaterials = adaptMaterials materials,
      gltfMeshes = adaptMeshes gltf buffers' meshes,
      gltfNodes = adaptNodes nodes,
      gltfSamplers = [],
      gltfTextures = []
    }

adaptAsset :: GlTF.Asset.Asset -> Asset
adaptAsset GlTF.Asset.Asset{..} = Asset
  { assetVersion = version,
    assetCopyright = copyright,
    assetGenerator = generator,
    assetMinVersion = minVersion
  }

adaptMaterials :: Maybe (Vector GlTF.Material.Material) -> Vector Material
adaptMaterials = maybe mempty (fmap adaptMaterial)

adaptMeshes
  :: GlTF.GlTF
  -> Vector GltfBuffer
  -> Maybe (Vector GlTF.Mesh.Mesh)
  -> Vector Mesh
adaptMeshes gltf buffers' = maybe mempty (fmap $ adaptMesh gltf buffers')

adaptNodes :: Maybe (Vector GlTF.Node.Node) -> Vector Node
adaptNodes = maybe mempty (fmap adaptNode)

adaptMaterial :: GlTF.Material.Material -> Material
adaptMaterial GlTF.Material.Material{..} = Material
  { materialAlphaCutoff = alphaCutoff,
    materialAlphaMode = adaptAlphaMode alphaMode,
    materialDoubleSided = doubleSided,
    materialEmissiveFactor = toV3 emissiveFactor,
    materialName = name,
    materialPbrMetallicRoughness = adaptPbrMetallicRoughness <$> pbrMetallicRoughness
  }

adaptMesh
  :: GlTF.GlTF
  -> Vector GltfBuffer
  -> GlTF.Mesh.Mesh
  -> Mesh
adaptMesh gltf buffers' GlTF.Mesh.Mesh{..} = Mesh
    { meshPrimitives = adaptMeshPrimitives gltf buffers' primitives,
      meshWeights = fromMaybe mempty weights,
      meshName = name
    }

adaptNode :: GlTF.Node.Node -> Node
adaptNode GlTF.Node.Node{..} = Node
  { nodeMeshId = GlTF.Mesh.unMeshIx <$> mesh,
    nodeName = name,
    nodeRotation = toV4 <$> rotation,
    nodeScale = toV3 <$> scale,
    nodeTranslation = toV3 <$> translation,
    nodeWeights = maybe [] toList weights
  }

adaptAlphaMode :: GlTF.Material.MaterialAlphaMode -> MaterialAlphaMode
adaptAlphaMode GlTF.Material.BLEND = Blend
adaptAlphaMode GlTF.Material.MASK = Mask
adaptAlphaMode GlTF.Material.OPAQUE = Opaque
adaptAlphaMode (GlTF.Material.MaterialAlphaMode alphaMode)
  = error $ "Invalid MaterialAlphaMode: " <> show alphaMode

adaptPbrMetallicRoughness
  :: GlTF.PbrMetallicRoughness.PbrMetallicRoughness
  -> PbrMetallicRoughness
adaptPbrMetallicRoughness GlTF.PbrMetallicRoughness.PbrMetallicRoughness{..}
  = PbrMetallicRoughness
    { pbrBaseColorFactor = toV4 baseColorFactor,
      pbrBaseColorTexture = adaptTextureInfo <$> baseColorTexture,
      pbrMetallicFactor = metallicFactor,
      pbrRoughnessFactor = roughnessFactor
    }

adaptMeshPrimitives
  :: GlTF.GlTF
  -> Vector GltfBuffer
  -> Vector GlTF.Mesh.MeshPrimitive
  -> Vector MeshPrimitive
adaptMeshPrimitives gltf = fmap . adaptMeshPrimitive gltf

adaptTextureInfo :: GlTF.TextureInfo.TextureInfo a -> TextureInfo
adaptTextureInfo GlTF.TextureInfo.TextureInfo{..} = TextureInfo
  { textureId = index,
    textureTexCoord = texCoord
  }

adaptMeshPrimitive
  :: GlTF.GlTF
  -> Vector GltfBuffer
  -> GlTF.Mesh.MeshPrimitive
  -> MeshPrimitive
adaptMeshPrimitive gltf buffers' GlTF.Mesh.MeshPrimitive{..} = MeshPrimitive
    { meshPrimitiveIndices = maybe mempty (vertexIndices gltf buffers') indices,
      meshPrimitiveMaterial = GlTF.Material.unMaterialIx <$> material,
      meshPrimitiveMode = adaptMeshPrimitiveMode mode,
      meshPrimitiveNormals = maybe mempty (vertexNormals gltf buffers') normals,
      meshPrimitivePositions = maybe mempty (vertexPositions gltf buffers') positions,
      meshPrimitiveTexCoords = maybe mempty (vertexTexCoords gltf buffers') texCoords
    }
    where positions = attributes HashMap.!? attributePosition
          normals = attributes HashMap.!? attributeNormal
          texCoords = attributes HashMap.!? attributeTexCoord
          

adaptMeshPrimitiveMode :: GlTF.Mesh.MeshPrimitiveMode -> MeshPrimitiveMode
adaptMeshPrimitiveMode = toEnum . GlTF.Mesh.unMeshPrimitiveMode

toV3 :: (a, a, a) -> V3 a
toV3 (x, y, z) = V3 x y z

toV4 :: (a, a, a, a) -> V4 a
toV4 (w, x, y, z) = V4 w x y z
