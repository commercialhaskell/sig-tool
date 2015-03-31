{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}

{-|
Module      : Sig.CLI.Update
Description : Haskell Package Signing Tool - CLI
Copyright   : (c) FPComplete.com, 2015
License     : BSD3
Maintainer  : Tim Dysinger <tim@fpcomplete.com>
Stability   : experimental
Portability : POSIX
-}

module Sig.CLI.Update where

import BasePrelude
import Control.Monad.IO.Class ( MonadIO )
import Sig.CLI.Types ( Options(Update) )

update :: forall m a.
          MonadIO m
       => Options -> m a
update Update = error "not implemented"
update _ = error "bad pattern match"
