{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Sig.Install
Description : Haskell Package Signing Tool: Installing with Cabal
Copyright   : (c) FPComplete.com, 2015
License     : BSD3
Maintainer  : Tim Dysinger <tim@fpcomplete.com>
Stability   : experimental
Portability : POSIX
-}

module Sig.Install where

import BasePrelude
import Sig.Cabal ( cabalInstall )
import Sig.Check ( check )
import Sig.Doc ( putHeader )

install :: [String] -> String -> IO ()
install extraArgs pkg =
  do check extraArgs pkg
     putHeader "Verifying Packages"
     cabalInstall extraArgs pkg
