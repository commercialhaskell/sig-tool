{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}

{-|
Module      : Sig.Init
Description : Haskell Package Signing Tool - CLI
Copyright   : (c) FPComplete.com, 2015
License     : BSD3
Maintainer  : Tim Dysinger <tim@fpcomplete.com>
Stability   : experimental
Portability : POSIX
-}

module Sig.Init where

import BasePrelude
import Control.Monad.IO.Class ( MonadIO(..) )
import System.Directory
    ( getHomeDirectory, createDirectoryIfMissing )
import System.FilePath ( (</>) )

initialize :: forall m a.
              MonadIO m
           => m a
initialize =
  liftIO (do homeDir <- getHomeDirectory
             createDirectoryIfMissing True
                                      (homeDir </> ".sig")
             initConfigFile)
  where initConfigFile = error "not implemented"
