module Command.GLTF.Loader.App where

import RIO
import RIO.Process

-- | Command line arguments
data Options = Options
  { optionsVerbose :: !Bool,
    optionsFile :: FilePath
  }

-- | Application state
data App = App
  { appLogFunc :: !LogFunc,
    appProcessContext :: !ProcessContext,
    appOptions :: !Options
  }

class HasOptions env where
  optionsL :: Lens' env Options

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })

instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x { appProcessContext = y })

instance HasOptions App where
  optionsL = lens appOptions (\app opts -> app { appOptions = opts })

_optionsVerbose :: Lens' Options Bool
_optionsVerbose = lens
  optionsVerbose
  (\opts verbose -> opts { optionsVerbose = verbose })

_optionsFile :: Lens' Options FilePath
_optionsFile = lens
  optionsFile
  (\opts file -> opts { optionsFile = file })
