-- |Transform a `Codec.GlTF.GlTF` to `Text.GLTF.Loader.Gltf.Gltf`
module Text.GLTF.Loader.Adapter
  ( attributePosition,
    attributeNormal,
    adaptGltf,
    adaptAsset,
    adaptMeshes,
    adaptNodes,
    adaptMesh,
    adaptNode,
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
import qualified Codec.GlTF.Mesh as GlTF.Mesh
import qualified Codec.GlTF.Node as GlTF.Node
import qualified Data.HashMap.Strict as HashMap

attributePosition :: Text
attributePosition = "POSITION"

attributeNormal :: Text
attributeNormal = "NORMAL"


adaptGltf :: GlTF.GlTF -> Vector GltfBuffer -> Gltf
adaptGltf gltf@GlTF.GlTF{..} buffers' = Gltf
    { gltfAsset = adaptAsset asset,
      gltfMeshes = adaptMeshes gltf buffers' meshes,
      gltfNodes = adaptNodes nodes
    }

adaptAsset :: GlTF.Asset.Asset -> Asset
adaptAsset GlTF.Asset.Asset{..} = Asset
  { assetVersion = version,
    assetCopyright = copyright,
    assetGenerator = generator,
    assetMinVersion = minVersion
  }

adaptMeshes
  :: GlTF.GlTF
  -> Vector GltfBuffer
  -> Maybe (Vector GlTF.Mesh.Mesh)
  -> Vector Mesh
adaptMeshes gltf buffers' = maybe mempty (fmap $ adaptMesh gltf buffers')

adaptNodes :: Maybe (Vector GlTF.Node.Node) -> Vector Node
adaptNodes = maybe mempty (fmap adaptNode)

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

adaptMeshPrimitives
  :: GlTF.GlTF
  -> Vector GltfBuffer
  -> Vector GlTF.Mesh.MeshPrimitive
  -> Vector MeshPrimitive
adaptMeshPrimitives gltf = fmap . adaptMeshPrimitive gltf

adaptMeshPrimitive
  :: GlTF.GlTF
  -> Vector GltfBuffer
  -> GlTF.Mesh.MeshPrimitive
  -> MeshPrimitive
adaptMeshPrimitive gltf buffers' GlTF.Mesh.MeshPrimitive{..} = MeshPrimitive
    { meshPrimitiveMode = adaptMeshPrimitiveMode mode,
      meshPrimitiveIndices = maybe mempty (vertexIndices gltf buffers') indices,
      meshPrimitivePositions = maybe mempty (vertexPositions gltf buffers') positions,
      meshPrimitiveNormals = []
    }
    where positions = attributes HashMap.!? attributePosition

adaptMeshPrimitiveMode :: GlTF.Mesh.MeshPrimitiveMode -> MeshPrimitiveMode
adaptMeshPrimitiveMode = toEnum . GlTF.Mesh.unMeshPrimitiveMode

toV3 :: (a, a, a) -> V3 a
toV3 (x, y, z) = V3 x y z

toV4 :: (a, a, a, a) -> V4 a
toV4 (w, x, y, z) = V4 w x y z
