{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

{-|
Module      : Sig.Cabal
Description : Haskell Package Signing Tool: Cabal Functions
Copyright   : (c) FPComplete.com, 2015
License     : BSD3
Maintainer  : Tim Dysinger <tim@fpcomplete.com>
Stability   : experimental
Portability : POSIX
-}
module Sig.Tool.Cabal
       (cabalFetch, cabalUpdate, getCabalCacheDir, getPackageIndexPath,
        getPackageTarballPath, packagesFromIndex)
       where

#if __GLASGOW_HASKELL__ < 710
import Prelude
#else
import Prelude hiding ((<$>))
#endif

import qualified Codec.Archive.Tar as Tar
       (EntryContent(NormalFile), Entry(entryContent),
        Entries(Done, Fail, Next), entryPath, read)
import Conduit
       (MonadIO(..), MonadThrow(..), MonadBaseControl, (=$), ($$),
        runResourceT, sourceFile, sinkList, linesUnboundedC, decodeUtf8C,
        concatMapC)
import Control.Monad (unless)
import qualified Data.ByteString.Lazy as BL (readFile)
import Data.List (intercalate, isSuffixOf, stripPrefix, sort)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Data.String (fromString)
import qualified Data.Text as T (unpack)
import Data.Version (Version(..))
import Distribution.Package
       (PackageName(PackageName), PackageIdentifier(PackageIdentifier),
        pkgName, pkgVersion)
import Distribution.Text (Text(disp), simpleParse)
import Sig.Tool.Types
import System.Directory (doesFileExist, getAppUserDataDirectory)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.Process (callProcess, readProcessWithExitCode)

cabalUpdate :: MonadIO m => m ()
cabalUpdate = liftIO (callProcess "cabal" ["update"])

cabalFetch
    :: (MonadIO m, MonadThrow m)
    => [String] -> PackageIdentifier -> m ()
cabalFetch opts (PackageIdentifier (PackageName name) (Version branch _tags)) = do
    let pkg = name <> "==" <> intercalate "." (map show branch)
    (code,_out,err) <-
        liftIO
            (readProcessWithExitCode
                 "cabal"
                 (["fetch"] ++ opts ++ [pkg])
                 mempty)
    unless (code == ExitSuccess) (throwM (CabalFetchException err))

packagesFromIndex
    :: (MonadIO m, MonadThrow m, MonadBaseControl IO m)
    => m [PackageIdentifier]
packagesFromIndex = do
    indexPath <- getPackageIndexPath
    indexExists <- liftIO (doesFileExist indexPath)
    unless
        indexExists
        (throwM
             (CabalIndexException
                  ("Cabal index \"" <> indexPath <>
                   "\" is missing. Please run `cabal update` first.")))
    filePathsFromTarball [] . Tar.read =<<
        liftIO . BL.readFile =<< getPackageIndexPath
  where
    filePathsFromTarball _ (Tar.Fail err) =
        throwM
            (CabalIndexException
                 ("Unable to read the Cabal package index: " <> show err))
    filePathsFromTarball pkgs Tar.Done = (return . sort . catMaybes) pkgs
    filePathsFromTarball pkgs (Tar.Next entry es) =
        case Tar.entryContent entry of
            Tar.NormalFile _ _
              | ".cabal" `isSuffixOf` Tar.entryPath entry ->
                  case splitOn "/" (Tar.entryPath entry) of
                      [] -> filePathsFromTarball pkgs es
                      [_] -> filePathsFromTarball pkgs es
                      (k:v:_) ->
                          filePathsFromTarball
                              (simpleParse (k <> "-" <> v) : pkgs)
                              es
            _ -> filePathsFromTarball pkgs es

getPackageIndexPath
    :: (MonadIO m, MonadThrow m, MonadBaseControl IO m)
    => m FilePath
getPackageIndexPath = do
    cabalCacheDir <- getCabalCacheDir
    return (cabalCacheDir </> "hackage.haskell.org" </> "00-index.tar")

getPackageTarballPath
    :: (MonadIO m, MonadThrow m, MonadBaseControl IO m)
    => PackageIdentifier -> m FilePath
getPackageTarballPath pkg = do
    let pName = (show . disp . pkgName) pkg
        pVersion = (show . disp . pkgVersion) pkg
    cabalCacheDir <- getCabalCacheDir
    return
        (cabalCacheDir </> "hackage.haskell.org" </> pName </> pVersion </>
         (pName <> "-" <> pVersion <> ".tar.gz"))

getCabalCacheDir
    :: (MonadIO m, MonadThrow m, MonadBaseControl IO m)
    => m FilePath
getCabalCacheDir = do
    c <- liftIO (getAppUserDataDirectory "cabal")
    configLines <-
        runResourceT $
        sourceFile (fromString (c </> "config")) $$ decodeUtf8C =$
        linesUnboundedC =$
        concatMapC (getRemoteCache . T.unpack) =$
        sinkList
    case configLines of
        [x] -> return x
        [] ->
            throwM
                (CabalIndexException
                     "No remote-repo-cache found in Cabal config file")
        _ ->
            throwM
                (CabalIndexException
                     "Multiple remote-repo-cache entries found in Cabal config file")
  where
    getRemoteCache s = do
        ("remote-repo-cache",stripPrefix ": " -> Just v) <-
            Just (break (== ':') s)
        Just v
