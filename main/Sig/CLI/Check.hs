{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}

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
import Control.Monad.IO.Class ( MonadIO )
import Sig.CLI.Types ( Options(Check) )

check :: forall m a.
         MonadIO m
      => Options -> m a
check (Check _package) = error "not implemented"
check _ = error "bad pattern match"
