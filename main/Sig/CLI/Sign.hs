{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Module      : Sig.CLI.Sign
Description : Haskell Package Signing Tool - CLI
Copyright   : (c) FPComplete.com, 2015
License     : BSD3
Maintainer  : Tim Dysinger <tim@fpcomplete.com>
Stability   : experimental
Portability : POSIX
-}

module Sig.CLI.Sign where

import BasePrelude
import Options.Applicative
import Sig.CLI.Types
import System.FilePath

data Sign = Sign

instance Command Sign where
  data CmdOpts Sign = SignCmd FilePath
  cmd Sign =
    command "sign"
            (info (helper <*>
                   (SignCliOpts <$>
                    argument str (metavar "PATH")))
                  (fullDesc <>
                   progDesc "Sign Package(s)"))
  run (SignCmd _path) = error "not implemented"
