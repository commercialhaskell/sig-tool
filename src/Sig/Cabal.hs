{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Sig.Cabal
Description : Haskell Package Signing Tool: Cabal Functions
Copyright   : (c) FPComplete.com, 2015
License     : BSD3
Maintainer  : Tim Dysinger <tim@fpcomplete.com>
Stability   : experimental
Portability : POSIX
-}

module Sig.Cabal where

import BasePrelude
import Data.List.Split ( splitOn )
import Distribution.Package
    ( PackageName(PackageName), PackageIdentifier(PackageIdentifier) )
import Sig.Types
    ( SigException(CabalFetchException, CabalInstallException,
                   CabalPackageListException) )
import System.Process
    ( waitForProcess, spawnProcess, readProcessWithExitCode )
import Distribution.PackageDescription
    ( PackageDescription(package),
      GenericPackageDescription(packageDescription) )
import Distribution.PackageDescription.Parse
    ( readPackageDescription )
import Distribution.Verbosity ( silent )

cabalInstallDryRun :: [String]
                   -> String
                   -> IO [PackageIdentifier]
cabalInstallDryRun opts pkg =
  do (code,out,err) <-
       readProcessWithExitCode
         "cabal"
         (["install","--dry-run"] ++
          opts ++
          [pkg])
         mempty
     if code /= ExitSuccess
        then throwIO (CabalPackageListException err)
        else return (if (head . reverse . lines $ out) ==
                        "Use --reinstall if you want to reinstall anyway."
                        then empty
                        else stdoutToPackageIdentifiers out)
  where stdoutToPackageIdentifiers :: String -> [PackageIdentifier]
        stdoutToPackageIdentifiers =
          map ((\(version:reverseName) ->
                  -- TODO use the PI parser
                  PackageIdentifier
                    (PackageName
                       (intercalate "-"
                                    (reverse reverseName)))
                    (Version {versionBranch =
                                map read (splitOn "." version)
                             ,versionTags = []})) . {- FIXME Deprecated: "See GHC ticket #2496" -}
               -- if people use 'tags' then this fn won't work, I'll
               -- have to parse smarter
               reverse .
               splitOn "-") .
          map (head .
               splitOn " ") .
          drop 2 .
          lines

cabalFetch :: [String] -> PackageIdentifier -> IO ()
cabalFetch opts (PackageIdentifier (PackageName name) (Version branch _tags)) =
  do let pkg =
           name <> "==" <>
           intercalate "."
                       (map show branch)
     (code,_out,err) <-
       readProcessWithExitCode
         "cabal"
         (["fetch"] ++
          opts ++
          [pkg])
         mempty
     if code /= ExitSuccess
        then throwIO (CabalFetchException err)
        else return ()

cabalInstall :: [String] -> String -> IO ()
cabalInstall opts pkg =
  do code <-
       waitForProcess =<<
       spawnProcess
         "cabal"
         (["install"] ++
          opts ++
          [pkg])
     if code /= ExitSuccess
        then throwIO (CabalInstallException "unable to cabal-install")
        else return ()

cabalFilePackageId :: FilePath -> IO PackageIdentifier
cabalFilePackageId fp =
  pure . package . packageDescription =<<
  readPackageDescription silent fp
