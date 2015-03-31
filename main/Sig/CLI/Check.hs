{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Module      : Sig.CLI.Check
Description : Haskell Package Signing Tool - CLI
Copyright   : (c) FPComplete.com, 2015
License     : BSD3
Maintainer  : Tim Dysinger <tim@fpcomplete.com>
Stability   : experimental
Portability : POSIX
-}

module Sig.CLI.Check where

import BasePrelude
import Options.Applicative
import Sig.CLI.Types

data Check = Check

instance Command Check where
  data CmdOpts Check = CheckCmd String
  cmd Check =
    command "check"
            (info (helper <*>
                   (CheckCliOpts <$>
                    argument str (metavar "PACKAGE")))
                  (fullDesc <>
                   progDesc "Check Package"))
  run (CheckCmd package) =
    error "not implemented"
