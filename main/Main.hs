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
      argument,
      strOption,
      long,
      short,
      showDefault,
      value )
import Sig.Check ( check )
import Sig.Init ( initialize )
import Sig.Install ( install )
import Sig.List ( list )
import Sig.Sign ( sign, signAll )
import Sig.Trust ( trust )
import Sig.Types ( exMsg )
import Sig.Update ( update )
import System.IO ( hPutStr, stderr )

-- | Main entry point.
main :: IO ExitCode
main =
  do args <- getArgs
     let (optParseArgs,extraArgs) =
           let (l,r) = span ("--" /=) args
           in (l,dropWhile ("--" ==) r)
     withArgs optParseArgs
              (do catch (do join (execOptParse extraArgs)
                            exitSuccess)
                        (\e ->
                           do hPutStr stderr ("ERROR: " <> exMsg e <> "\n")
                              exitFailure))

execOptParse :: [String] -> IO (IO ())
execOptParse extraArgs =
  execParser
    (info (helper <*>
           subparser (command "check"
                              (info (helper <*>
                                     (check extraArgs <$>
                                      argument str (metavar "PACKAGE")))
                                    (fullDesc <>
                                     progDesc "Check Package")) <>
                      command "init"
                              (info (helper <*> pure initialize)
                                    (fullDesc <>
                                     progDesc "Initialize")) <>
                      command "install"
                              (info (helper <*>
                                     (install extraArgs <$>
                                      argument str (metavar "PACKAGE")))
                                    (fullDesc <>
                                     progDesc "Install Package")) <>
                      command "mappings"
                              (info (helper <*> pure list)
                                    (fullDesc <>
                                     progDesc "List Mappings")) <>
                      command "sign"
                              (info (helper <*>
                                     (sign <$> url <*>
                                      argument str (metavar "PATH")))
                                    (fullDesc <>
                                     progDesc "Sign a sdist Tarball")) <>
                      command "sign-all"
                              (info (helper <*>
                                     (signAll <$> url <*>
                                      argument str (metavar "USER")))
                                    (fullDesc <>
                                     progDesc "Sign Your Hackage Packages")) <>
                      command "trust"
                              (info (helper <*>
                                     (trust <$>
                                      argument str (metavar "FINGERPRINT") <*>
                                      argument str (metavar "EMAIL")))
                                    (fullDesc <>
                                     progDesc "Trust Mappings")) <>
                      command "update"
                              (info (helper <*> pure update)
                                    (fullDesc <>
                                     progDesc "Update the Archive"))))
          (fullDesc <>
           header ("sig " <> packageVersion <> " " <> buildDate) <>
           progDesc "Haskell Package Signing Tool"))
  where url =
          strOption (long "url" <>
                     short 'u' <>
                     metavar "URL" <>
                     showDefault <>
                     value "https://sig.commercialhaskell.org")
packageVersion :: String
packageVersion =
  $(packageVariable (pkgVersion . package))

buildDate :: String
buildDate =
  $(stringE =<<
    runIO (show `fmap` Data.Time.getCurrentTime))
