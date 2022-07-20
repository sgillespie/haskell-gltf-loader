module Command.GLTF.Loader.App where

import RIO
import RIO.Process

-- | Command line arguments
data Options = Options
  { optionsVerbose :: !Bool
  }

-- | Application state
data App = App
  { appLogFunc :: !LogFunc,
    appProcessContext :: !ProcessContext,
    appOptions :: !Options
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })

instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x { appProcessContext = y })
