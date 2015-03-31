{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Module      : Sig.CLI.Trust
Description : Haskell Package Signing Tool - CLI
Copyright   : (c) FPComplete.com, 2015
License     : BSD3
Maintainer  : Tim Dysinger <tim@fpcomplete.com>
Stability   : experimental
Portability : POSIX
-}

module Sig.CLI.Trust where

import BasePrelude
import Options.Applicative
import Sig.CLI.Types

data Trust = Trust

instance Command Trust where
  data CmdOpts Trust = TrustCmd String
  cmd Trust =
    command "trust"
          (info (helper <*>
                 (TrustCliOpts <$>
                  argument str (metavar "NAME")))
                (fullDesc <>
                 progDesc "Trust Mappings"))
  run (TrustCmd _name) = error "not implemented"
