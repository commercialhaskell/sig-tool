{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Module      : Main
Description : Haskell Package Signing Tool - CLI
Copyright   : (c) FPComplete.com, 2015
License     : BSD3
Maintainer  : Tim Dysinger <tim@fpcomplete.com>
Stability   : experimental
Portability : POSIX
-}

module Main where

import BasePrelude

import Control.Monad.IO.Class
import Data.Time
import Distribution.PackageDescription.TH
import Language.Haskell.TH
import Options.Applicative

import Sig.CLI.Check
import Sig.CLI.Init
import Sig.CLI.Install
import Sig.CLI.List
import Sig.CLI.Sign
import Sig.CLI.Trust
import Sig.CLI.Types
import Sig.CLI.Update

-- | Main entry point.
main :: IO ()
main =
  execParser
    (info (helper <*>
           subparser (cmd Init))
          (fullDesc <>
           header ("sig " <> packageVersion <> " " <> buildDate) <>
           progDesc "Haskell Package Signing Tool")) >>=
  go

go :: MonadIO m
   => CliOpts -> m b
go (CheckCliOpts a) = run (CheckCmd a)
go InitCliOpts = run InitCmd
go (InstallCliOpts a) = run (InstallCmd a)
go ListCliOpts = run ListCmd
go (SignCliOpts a) = run (SignCmd a)
go (TrustCliOpts a) = run (TrustCmd a)
go UpdateCliOpts = run UpdateCmd

packageVersion :: String
packageVersion =
  $(packageVariable (pkgVersion . package))

buildDate :: String
buildDate =
  $(stringE =<<
    runIO (show `fmap` Data.Time.getCurrentTime))
