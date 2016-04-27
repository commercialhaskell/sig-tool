{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Sig.Tool
Description : Bulk Haskell Package Signing Tool
Copyright   : (c) FPComplete.com, 2015
License     : BSD3
Maintainer  : Tim Dysinger <tim@fpcomplete.com>
Stability   : experimental
Portability : POSIX
-}

module Sig.Tool (setup, sign) where

import Control.Monad.Catch (MonadMask, MonadThrow, throwM)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Logger (MonadLogger, logInfo)
import Control.Monad.Trans.Control (MonadBaseControl)
import Crypto.Hash.SHA256 (hashlazy)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as M
import Data.Monoid ((<>))
import qualified Data.Text as T
import Data.Traversable (forM)
import qualified Data.Yaml as Y
import qualified Distribution.Package as Cabal
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Path
import Sig.Tool.Cabal
import Sig.Tool.Hackage
import Sig.Tool.Types
import qualified Stack.Sig.Sign as Stack
import System.Directory
       (copyFile, createDirectoryIfMissing, getCurrentDirectory)
import System.Environment (unsetEnv)
import Text.Printf (printf)

setup
    :: (MonadBaseControl IO m, MonadIO m, MonadLogger m, MonadThrow m)
    => String -> m ()
setup uname = do
    (packagesPath,manifestPath) <- getPaths
    liftIO (unsetEnv "GHC_PACKAGE_PATH")
    liftIO cabalUpdate
    fromHackage <- packagesForMaintainer uname
    fromIndex <- packagesFromIndex
    liftIO (createDirectoryIfMissing True (toFilePath packagesPath))
    packages <-
        forM
            (filter
                 (\x ->
                       Cabal.pkgName x `elem` map Cabal.pkgName fromHackage)
                 fromIndex)
            (\pkg ->
                  (do liftIO (cabalFetch ["--no-dependencies"] pkg)
                      cabalPackagePath <-
                          parseAbsFile =<< liftIO (getPackageTarballPath pkg)
                      let cabalPackageFileName = filename cabalPackagePath
                          destPackagePath =
                              packagesPath </> cabalPackageFileName
                      cabalPackageHash <- digest cabalPackagePath
                      liftIO
                          (copyFile
                               (toFilePath cabalPackagePath)
                               (toFilePath destPackagePath))
                      $logInfo
                          ((T.pack . toFilePath) cabalPackageFileName <> " (" <>
                           T.pack cabalPackageHash <>
                           ")")
                      return
                          ( toFilePath (filename destPackagePath)
                          , cabalPackageHash)))
    liftIO (Y.encodeFile (toFilePath manifestPath) (M.fromList packages))

sign
    :: (MonadIO m, MonadLogger m, MonadMask m, MonadThrow m)
    => String -> m ()
sign url = do
    (packagesPath,manifestPath) <- getPaths
    mPackagesMap <- liftIO (Y.decodeFile (toFilePath manifestPath))
    case mPackagesMap :: Maybe (M.Map FilePath String) of
        Nothing -> throwM ManifestParseException
        Just packageMap -> do
            manager <- liftIO (newManager tlsManagerSettings)
            mapM_
                (\(path,hash) ->
                      do cabalPackagePath <-
                             fmap (packagesPath </>) (parseRelFile path)
                         cabalPackageHash <- digest cabalPackagePath
                         if cabalPackageHash /= hash
                             then throwM DigestMismatchException
                             else Stack.sign manager url cabalPackagePath)
                (M.toList packageMap)

getPaths :: (MonadIO m, MonadThrow m) => m (Path Abs Dir, Path Abs File)
getPaths = do
    currentPath <- parseAbsDir =<< liftIO getCurrentDirectory
    let packagesPath = currentPath </> $(mkRelDir "packages")
        manifestPath = packagesPath </> $(mkRelFile "manifest.yaml")
    return (packagesPath, manifestPath)

digest
    :: MonadIO m
    => Path Abs File -> m String
digest path = fmap (toHex . hashlazy) (liftIO (LBS.readFile (toFilePath path)))
  where
    toHex bytes = BS.unpack bytes >>= printf "%02x"
