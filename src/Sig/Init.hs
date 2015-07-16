{-|
Module      : Sig.Init
Description : Haskell Package Signing Tool: Initialize
Copyright   : (c) FPComplete.com, 2015
License     : BSD3
Maintainer  : Tim Dysinger <tim@fpcomplete.com>
Stability   : experimental
Portability : POSIX
-}

module Sig.Init where

import Sig.Config ( defaultConfig, writeConfigIfMissing )
import Sig.Update ( update )

initialize :: String -> IO ()
initialize url =
  do writeConfigIfMissing defaultConfig
     update url
