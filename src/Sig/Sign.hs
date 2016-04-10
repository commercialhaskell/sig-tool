{-# LANGUAGE FlexibleContexts #-}

{-|
Module      : Sig.Sign
Description : Bulk Haskell Package Signing Tool: Sign
Copyright   : (c) FPComplete.com, 2015
License     : BSD3
Maintainer  : Tim Dysinger <tim@fpcomplete.com>
Stability   : experimental
Portability : POSIX
-}

module Sig.Sign (setup, sign) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStdoutLoggingT)
import Data.Foldable (forM_)
import qualified Distribution.Package as Cabal
import Path
import Sig.Cabal
       (cabalFetch, packagesFromIndex, getPackageTarballPath)
import Sig.Hackage (packagesForMaintainer)
import qualified Stack.Sig.Sign as Stack
import Stack.Types

setup :: String -> IO ()
setup uname =
    runStdoutLoggingT
        (do fromHackage <- packagesForMaintainer uname
            fromIndex <- packagesFromIndex
            forM_
                (filter
                     (\x ->
                           (Cabal.pkgName x) `elem`
                           (map Cabal.pkgName fromHackage))
                     fromIndex)
                (\pkg ->
                      (do liftIO (cabalFetch ["--no-dependencies"] pkg)
                          filePath <- liftIO (getPackageTarballPath pkg)
                          return ())))

sign :: String -> IO ()
sign url =
    runStdoutLoggingT
        (do let uname = "dysinger"
                fromIndex = undefined
                fromHackage = undefined
            forM_
                (filter
                     (\x ->
                           (Cabal.pkgName x) `elem`
                           (map Cabal.pkgName fromHackage))
                     fromIndex)
                (\pkg ->
                      (do liftIO (cabalFetch ["--no-dependencies"] pkg)
                          filePath <-
                              parseAbsFile =<<
                              liftIO (getPackageTarballPath pkg)
                          Stack.signPackage
                              url
                              (PackageIdentifier
                                   (fromCabalPackageName $ Cabal.pkgName pkg)
                                   (fromCabalVersion $ Cabal.pkgVersion pkg))
                              filePath)))
