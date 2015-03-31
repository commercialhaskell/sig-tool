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
import Data.Time ( getCurrentTime )
import Distribution.PackageDescription.TH
    ( PackageDescription(package),
      PackageIdentifier(pkgVersion),
      packageVariable )
import Language.Haskell.TH ( runIO, stringE )
import Options.Applicative
    ( helper,
      execParser,
      subparser,
      str,
      progDesc,
      metavar,
      info,
      header,
      fullDesc,
      command,
      argument )
import Sig.CLI.Check ( check )
import Sig.CLI.Init ( initialize )
import Sig.CLI.Install ( install )
import Sig.CLI.List ( list )
import Sig.CLI.Sign ( sign )
import Sig.CLI.Trust ( trust )
import Sig.CLI.Update ( update )

-- | Main entry point.
main :: IO ()
main =
  join (execParser
          (info (helper <*>
                 subparser (command "check"
                                    (info (helper <*>
                                           (check <$>
                                            argument str (metavar "PACKAGE")))
                                          (fullDesc <>
                                           progDesc "Check Package")) <>
                            command "init"
                                    (info (helper <*> pure initialize)
                                          (fullDesc <>
                                           progDesc "Initialize")) <>
                            command "install"
                                    (info (helper <*>
                                           (install <$>
                                            argument str (metavar "PACKAGE")))
                                          (fullDesc <>
                                           progDesc "Install Package")) <>
                            command "mappings"
                                    (info (helper <*> pure list)
                                          (fullDesc <>
                                           progDesc "List Mappings")) <>
                            command "sign"
                                    (info (helper <*>
                                           (sign <$>
                                            argument str (metavar "PATH")))
                                          (fullDesc <>
                                           progDesc "Sign Package(s)")) <>
                            command "trust"
                                    (info (helper <*>
                                           (trust <$>
                                            argument str (metavar "NAME")))
                                          (fullDesc <>
                                           progDesc "Trust Mappings")) <>
                            command "update"
                                    (info (helper <*> pure update)
                                          (fullDesc <>
                                           progDesc "Update the Archive"))))
                (fullDesc <>
                 header ("sig " <> packageVersion <> " " <> buildDate) <>
                 progDesc "Haskell Package Signing Tool")))

packageVersion :: String
packageVersion =
  $(packageVariable (pkgVersion . package))

buildDate :: String
buildDate =
  $(stringE =<<
    runIO (show `fmap` Data.Time.getCurrentTime))
