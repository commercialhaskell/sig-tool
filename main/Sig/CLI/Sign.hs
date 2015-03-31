{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}

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
import Control.Monad.IO.Class
import Sig.CLI.Types

sign :: forall m a.
        MonadIO m
     => Options -> m a
sign (Sign _path) = error "not implemented"
sign _ = error "bad pattern match"
