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
              (catch (do join (execOptParse extraArgs)
                         exitSuccess)
                     (\e ->
                        do hPutStr stderr
                                   ("ERROR: " <>
                                    show (e :: SomeException) <>
                                    "\n")
                           exitFailure))

execOptParse :: [String] -> IO (IO ())
execOptParse extraArgs =
  execParser
    (info (helper <*>
           subparser (checkCmd <> initCmd <> installCmd <> mappingsCmd <>
                      signCmd <> trustCmd <> updateCmd))
          (fullDesc <>
           header ("sig " <> packageVersion <> " " <> buildDate) <>
           progDesc "Haskell Package Signing Tool"))
  where checkCmd =
          command "check"
                  (info (helper <*>
                         (check extraArgs <$>
                          argument str (metavar "PACKAGE")))
                        (fullDesc <>
                         progDesc "Check a package"))
        initCmd =
          command "init"
                  (info (helper <*>
                         (initialize <$> url))
                        (fullDesc <>
                         progDesc "Initialize"))
        installCmd =
          command "install"
                  (info (helper <*>
                         (install extraArgs <$>
                          argument str (metavar "PACKAGE")))
                        (fullDesc <>
                         progDesc "Install package"))
        mappingsCmd =
          command "mappings"
                  (info (helper <*> pure list)
                        (fullDesc <>
                         progDesc "List mappings"))
        signCmd =
          command "sign"
                  (info (helper <*>
                         (subparser (command "sdist"
                                             (info (helper <*>
                                                    (sign <$> url <*>
                                                     argument str (metavar "PATH")))
                                                   (fullDesc <>
                                                    progDesc "Sign a single sdist tarball")) <>
                                     command "hackage"
                                             (info (helper <*>
                                                    (signAll <$> url <*>
                                                     argument str (metavar "USER")))
                                                   (fullDesc <>
                                                    progDesc "Sign all your Hackage packages")))))
                        (fullDesc <>
                         progDesc "Sign packages"))
        trustCmd =
          command "trust"
                  (info (helper <*>
                         (trust <$>
                          argument str (metavar "FINGERPRINT") <*>
                          argument str (metavar "EMAIL")))
                        (fullDesc <>
                         progDesc "Trust mappings"))
        updateCmd =
          command "update"
                  (info (helper <*>
                         (update <$> url))
                        (fullDesc <>
                         progDesc "Update the archive"))
        url =
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
