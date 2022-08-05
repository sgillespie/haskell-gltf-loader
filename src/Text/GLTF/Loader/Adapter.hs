-- |Transform a `Codec.GlTF.GlTF` to `Text.GLTF.Loader.Gltf.Gltf`
module Text.GLTF.Loader.Adapter
  ( adaptGltf,
    adaptAsset,
    adaptMeshes,
    adaptNodes,
    adaptMesh,
    adaptNode,
    adaptMeshPrimitives,
    adaptMeshPrimitive,
    adaptMeshPrimitiveMode
  ) where

import Text.GLTF.Loader.Gltf

import Linear (V3(..), V4(..))
import RIO
import RIO.Partial (toEnum)
import qualified Codec.GlTF as GlTF
import qualified Codec.GlTF.Asset as GlTF.Asset
import qualified Codec.GlTF.Mesh as GlTF.Mesh
import qualified Codec.GlTF.Node as GlTF.Node

adaptGltf :: GlTF.GlTF -> Gltf
adaptGltf GlTF.GlTF{..} = Gltf
  { gltfAsset = adaptAsset asset,
    gltfMeshes = adaptMeshes meshes,
    gltfNodes = adaptNodes nodes
  }

adaptAsset :: GlTF.Asset.Asset -> Asset
adaptAsset GlTF.Asset.Asset{..} = Asset
  { assetVersion = version,
    assetCopyright = copyright,
    assetGenerator = generator,
    assetMinVersion = minVersion
  }

adaptMeshes :: Maybe (Vector GlTF.Mesh.Mesh) -> [Mesh]
adaptMeshes = maybe [] (map adaptMesh . toList)

adaptNodes :: Maybe (Vector GlTF.Node.Node) -> [Node]
adaptNodes = maybe [] (map adaptNode . toList)

adaptMesh :: GlTF.Mesh.Mesh -> Mesh
adaptMesh GlTF.Mesh.Mesh{..} = Mesh
  { meshPrimitives = adaptMeshPrimitives primitives,
    meshWeights = maybe [] toList weights,
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

adaptMeshPrimitives :: Vector GlTF.Mesh.MeshPrimitive -> [MeshPrimitive]
adaptMeshPrimitives = map adaptMeshPrimitive . toList

adaptMeshPrimitive :: GlTF.Mesh.MeshPrimitive -> MeshPrimitive
adaptMeshPrimitive GlTF.Mesh.MeshPrimitive{..} = MeshPrimitive
  { meshPrimitiveMode = adaptMeshPrimitiveMode mode,
    vertexIndices = [],
    vertexPositions = [],
    vertexNormals = []
  }

adaptMeshPrimitiveMode :: GlTF.Mesh.MeshPrimitiveMode -> MeshPrimitiveMode
adaptMeshPrimitiveMode = toEnum . GlTF.Mesh.unMeshPrimitiveMode

toV3 :: (a, a, a) -> V3 a
toV3 (x, y, z) = V3 x y z

toV4 :: (a, a, a, a) -> V4 a
toV4 (w, x, y, z) = V4 w x y z
