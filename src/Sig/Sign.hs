{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}

{-|
Module      : Sig.Sign
Description : Haskell Package Signing Tool - CLI
Copyright   : (c) FPComplete.com, 2015
License     : BSD3
Maintainer  : Tim Dysinger <tim@fpcomplete.com>
Stability   : experimental
Portability : POSIX
-}

module Sig.Sign where

import BasePrelude
import Control.Monad.IO.Class ( MonadIO )

sign :: forall m a.
        MonadIO m
     => FilePath -> m a
sign _path = error "not implemented"
