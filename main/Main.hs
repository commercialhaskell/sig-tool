{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Main
Description : Haskell Package Signing Tool: Main
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
import Sig.Sign ( sign, signAll )

-- | Main entry point.
main :: IO ()
main =
  join (execParser
          (info (helper <*>
                 subparser (command "sdist"
                                    (info (helper <*>
                                           (sign <$>
                                            argument str (metavar "PATH")))
                                          (fullDesc <>
                                           progDesc "Sign a single sdist tarball")) <>
                            command "hackage"
                                    (info (helper <*>
                                           (signAll <$>
                                            argument str (metavar "USER")))
                                          (fullDesc <>
                                           progDesc "Sign all your Hackage packages"))))
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
