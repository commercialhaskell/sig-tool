{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Module      : Sig.CLI.Install
Description : Haskell Package Signing Tool - CLI
Copyright   : (c) FPComplete.com, 2015
License     : BSD3
Maintainer  : Tim Dysinger <tim@fpcomplete.com>
Stability   : experimental
Portability : POSIX
-}

module Sig.CLI.Install where

import BasePrelude
import Options.Applicative
import Sig.CLI.Types

data Install = Install

instance Command Install where
  data CmdOpts Install = InstallCmd String
  cmd Install =
    command "install"
            (info (helper <*>
                   (InstallCliOpts <$>
                    argument str (metavar "PACKAGE")))
                  (fullDesc <>
                   progDesc "Install Package"))
  run (InstallCmd package) =
    error "not implemented"
