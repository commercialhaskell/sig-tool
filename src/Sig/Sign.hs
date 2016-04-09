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

import Control.Exception (throwIO)
import Control.Monad (when)
import Control.Monad.Catch
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (MonadLogger(..))
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Foldable (forM_)
import Data.Monoid ((<>))
import qualified Data.Text as T (unpack)
import Data.Version (showVersion)
import qualified Distribution.Package as Cabal
import Network.HTTP.Conduit
       (Response(responseStatus), RequestBody(RequestBodyBS),
        Request(method, requestBody), withManager, httpLbs, parseUrl)
import Network.HTTP.Types (status200, methodPut)
import Path
import Sig.Cabal
       (cabalFetch, packagesFromIndex, getPackageTarballPath)
import Sig.Hackage (packagesForMaintainer)
import qualified Stack.Sig.GPG as Stack
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
