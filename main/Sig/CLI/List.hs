{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Module      : Sig.CLI.List
Description : Haskell Package Signing Tool - CLI
Copyright   : (c) FPComplete.com, 2015
License     : BSD3
Maintainer  : Tim Dysinger <tim@fpcomplete.com>
Stability   : experimental
Portability : POSIX
-}

module Sig.CLI.List where

import BasePrelude
import Options.Applicative
import Sig.CLI.Types

data List = List

instance Command List where
  data CmdOpts List = ListCmd
  cmd List =
    command "mappings"
            (info (helper <*>
                   pure ListCliOpts)
                  (fullDesc <>
                   progDesc "List Mappings"))
  run ListCmd = error "not implemented"
