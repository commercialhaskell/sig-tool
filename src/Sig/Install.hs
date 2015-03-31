{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}

{-|
Module      : Sig.Install
Description : Haskell Package Signing Tool - CLI
Copyright   : (c) FPComplete.com, 2015
License     : BSD3
Maintainer  : Tim Dysinger <tim@fpcomplete.com>
Stability   : experimental
Portability : POSIX
-}

module Sig.Install where

import BasePrelude
import Control.Monad.IO.Class ( MonadIO )

install :: forall m a.
           MonadIO m
        => String -> m a
install _package = error "not implemented"
