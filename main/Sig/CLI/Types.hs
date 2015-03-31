{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Module      : Sig.CLI.Types
Description : Haskell Package Signing Tool - CLI
Copyright   : (c) FPComplete.com, 2015
License     : BSD3
Maintainer  : Tim Dysinger <tim@fpcomplete.com>
Stability   : experimental
Portability : POSIX
-}

module Sig.CLI.Types where

import BasePrelude
import Control.Monad.IO.Class
import Options.Applicative

data CliOpts
  = InitCliOpts
  | UpdateCliOpts
  | SignCliOpts FilePath
  | ListCliOpts
  | TrustCliOpts String
  | CheckCliOpts String
  | InstallCliOpts String

class Command a where
  data CmdOpts a :: *
  cmd :: a -> Mod CommandFields CliOpts
  run :: forall m b. MonadIO m => CmdOpts a -> m b
