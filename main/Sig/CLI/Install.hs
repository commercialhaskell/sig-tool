{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}

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
import Control.Monad.IO.Class ( MonadIO )
import Sig.CLI.Types ( Options(Install) )

install :: forall m a.
           MonadIO m
        => Options -> m a
install (Install _package) = error "not implemented"
install _ = error "bad pattern match"
