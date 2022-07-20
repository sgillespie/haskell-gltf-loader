module Command.GLTF.Loader.Run (run) where

import Command.GLTF.Loader.App

import RIO

run :: RIO App ()
run = do
  logInfo "We're inside the application!"
