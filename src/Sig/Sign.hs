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
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Foldable (forM_)
import Data.Monoid ((<>))
import qualified Data.Text as T (unpack)
import Data.Version (showVersion)
import Distribution.Package
       (PackageName(PackageName), PackageIdentifier(pkgName, pkgVersion))
import Network.HTTP.Conduit
       (Response(responseStatus), RequestBody(RequestBodyBS),
        Request(method, requestBody), withManager, httpLbs, parseUrl)
import Network.HTTP.Types (status200, methodPut)
import Sig.Cabal
       (cabalFetch, packagesFromIndex, getPackageTarballPath)
import qualified Sig.GPG as GPG (fullFingerprint, sign, verifyFile)
import Sig.Hackage (packagesForMaintainer)
import Stack.Types.Sig

setup
    :: (MonadIO m, MonadThrow m, MonadBaseControl IO m)
    => String -> m ()
setup uname = do
    fromHackage <- packagesForMaintainer uname
    fromIndex <- packagesFromIndex
    forM_
        (filter
             (\x ->
                   (pkgName x) `elem` (map pkgName fromHackage))
             fromIndex)
        (\pkg ->
              liftIO
                  (do cabalFetch ["--no-dependencies"] pkg
                      filePath <- getPackageTarballPath pkg
                      print filePath))

sign
    :: (MonadIO m, MonadThrow m, MonadBaseControl IO m)
    => String -> m ()
sign url = do
    let uname = "dysinger"
    fromHackage <- packagesForMaintainer uname
    fromIndex <- packagesFromIndex
    forM_
        (filter
             (\x ->
                   (pkgName x) `elem` (map pkgName fromHackage))
             fromIndex)
        (\pkg ->
              liftIO
                  (do cabalFetch ["--no-dependencies"] pkg
                      filePath <- getPackageTarballPath pkg
                      signPackage url pkg filePath
                      ))

-- TODO replace this with the Stack.Sig.Sign.signPackage fn

signPackage :: String -> PackageIdentifier -> FilePath -> IO ()
signPackage url pkg filePath = do
    sig@(Signature signature) <- GPG.sign filePath
    let (PackageName name) = pkgName pkg
        version = showVersion (pkgVersion pkg)
    fingerprint <- GPG.verifyFile sig filePath >>= GPG.fullFingerprint
    req <-
        parseUrl
            (url <> "/upload/signature/" <> name <> "/" <> version <> "/" <>
             T.unpack (fingerprintSample fingerprint))
    let put =
            req
            { method = methodPut
            , requestBody = RequestBodyBS signature
            }
    res <- withManager (httpLbs put)
    when
        (responseStatus res /= status200)
        (throwIO (GPGSignException "unable to sign & upload package"))
