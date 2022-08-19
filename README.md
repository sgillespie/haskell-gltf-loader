# Haskell GlTF Loader

[![pipeline status](https://gitlab.com/sgillespie/haskell-gltf-loader/badges/main/pipeline.svg)](https://gitlab.com/sgillespie/haskell-gltf-loader/-/commits/main)

> A high level GlTF loader


## Prerequisites

In order to build or install you will need

 * [GHC](https://www.haskell.org/downloads/) (Tested on 9.0.2)
 * [Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/) (Tested on 2.7.5)

## Building

Build the project

    stack setup
    stack build

Run the tests (if desired)

```
stack test
```

## API Documentation
To build the documentation, run

    stack haddock

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

Currently, only retreiving indices are supported, but we hope to support the majority of GlTF
features:

 - [ ] Animitions
 - [ ] Asset
 - [ ] Cameras
 - [ ] Images
 - [ ] Materials
 - [x] Meshes
    - [x] Positions
    - [x] Indices
    - [ ] Normals
    - [ ] Texture Coordinates
 - [ ] Node
 - [ ] Samplers
 - [ ] Skins

## Authors

Sean Gillespie <sean@mistersg.net>

## Acknowledgements

This project is largely based on [https://hackage.haskell.org/package/gltf-codec](gltf-codec) by 
Alexander Bondarenko

## License
This project is licensed under the MIT License. See [LICENSE](LICENSE)

