{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}

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
import Control.Monad.IO.Class ( MonadIO )
import Sig.CLI.Types ( Options(Trust) )

trust :: forall m a.
        MonadIO m
     => Options -> m a
trust (Trust _name) = error "not implemented"
trust _ = error "bad pattern match"
