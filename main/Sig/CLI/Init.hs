{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Module      : Sig.CLI.Init
Description : Haskell Package Signing Tool - CLI
Copyright   : (c) FPComplete.com, 2015
License     : BSD3
Maintainer  : Tim Dysinger <tim@fpcomplete.com>
Stability   : experimental
Portability : POSIX
-}

module Sig.CLI.Init where

import BasePrelude
import Control.Monad.IO.Class ( liftIO )
import Options.Applicative
import Sig.CLI.Types
import System.Directory
import System.FilePath

data Init = Init

instance Command Init where
  data CmdOpts Init = InitCmd
  cmd Init =
    command "init"
            (info (helper <*>
                   pure InitCliOpts)
                  (fullDesc <>
                   progDesc "Initialize"))
  run InitCmd =
    liftIO (do homeDir <- getHomeDirectory
               createDirectoryIfMissing True
                                        (homeDir </> ".sig")
               initConfigFile)
    where initConfigFile =
            error "not implemented"
