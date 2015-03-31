{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}

{-|
Module      : Sig.CLI.List
Description : Haskell Package Signing Tool - CLI
Copyright   : (c) FPComplete.com, 2015
License     : BSD3
Maintainer  : Tim Dysinger <tim@fpcomplete.com>
Stability   : experimental
Portability : POSIX
-}

module Sig.CLI.List where

import BasePrelude
import Control.Monad.IO.Class ( MonadIO )
import Sig.CLI.Types ( Options(List) )

list :: forall m a.
        MonadIO m
     => Options -> m a
list List = error "not implemented"
list _ = error "bad pattern match"
