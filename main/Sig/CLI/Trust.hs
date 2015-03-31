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

trust :: forall m a.
        MonadIO m
     => String -> m a
trust _name = error "not implemented"
