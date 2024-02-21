# Haskell GlTF Loader

[![pipeline status](https://gitlab.com/sgillespie/haskell-gltf-loader/badges/main/pipeline.svg)](https://gitlab.com/sgillespie/haskell-gltf-loader/-/commits/main)

> A high level GlTF loader

## Prerequisites

In order to build or install you will need

 * [GHC](https://www.haskell.org/downloads/) (Tested on 9.4.8), or
 * [Nix](https://nixos.org/download)

## Building

If using Nix, start up a development shell (otherwise omit this step)

    nix develop .

Build the project

    cabal build

Run the tests (if desired)

    cabal test

## API Documentation
To build the documentation, run

    cabal haddock

## API Examples

Use `fromFile` or `fromBytestring` to load a GlTF scene


    import Text.GLTF.Loader (fromFile)

    -- ...
    -- Load a GLTF scene from a file
    loadGltfFile :: IO (Either Errors Gltf)
    loadGltfFile = fromFile "./cube.gltf"

Then you can, for example, get all the vertices:

    getVertices :: Gltf -> [V3 Float]
    getVertices gltf = concatMap getVertices' (gltf ^. _meshes)
      where getVertices' mesh = concatMap (^. _meshPrimitivePositions) (mesh ^. _meshPrimitives)

## CLI

This includes a CLI utility to inspect GlTF files

    gltf-loader --help

## Roadmap

Currently, only geometries and PBR materials are supported, but I hope to support the
majority of GlTF features:

 - [ ] Animations
 - [x] Asset
 - [ ] Cameras
 - [x] Images
 - [x] Materials
   - [x] PBR Metallic Roughness
   - [x] Textures
 - [x] Meshes
    - [x] Positions
    - [x] Indices
    - [x] Normals
    - [x] Texture Coordinates
 - [x] Nodes
 - [x] Samplers
 - [ ] Skins

## Authors

Sean Gillespie <sean@mistersg.net>

## Acknowledgements

This project is largely based on [https://hackage.haskell.org/package/gltf-codec](gltf-codec) by
Alexander Bondarenko

## License
This project is licensed under the MIT License. See [LICENSE](LICENSE)
