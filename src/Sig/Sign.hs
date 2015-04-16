{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Sig.Sign
Description : Haskell Package Signing Tool: Signing Packages
Copyright   : (c) FPComplete.com, 2015
License     : BSD3
Maintainer  : Tim Dysinger <tim@fpcomplete.com>
Stability   : experimental
Portability : POSIX
-}

module Sig.Sign where

import BasePrelude
import qualified Data.Text as T ( unpack )
import Data.UUID ( toString )
import Data.UUID.V4 ( nextRandom )
import Distribution.Package
    ( PackageName(PackageName),
      PackageIdentifier(pkgName, pkgVersion) )
import Network.HTTP.Conduit
    ( Response(responseStatus),
      RequestBody(RequestBodyBS),
      Request(method, requestBody),
      withManager,
      httpLbs,
      parseUrl )
import Network.HTTP.Types ( status200, methodPut )
import Sig.Cabal ( cabalFilePackageId )
import qualified Sig.GPG as GPG ( sign, fingerprintFromVerify )
import Sig.Types
    ( SigException(GPGSignException),
      FingerprintSample(fingerprintSample),
      Signature(Signature) )
import System.Directory
    ( getTemporaryDirectory,
      getDirectoryContents,
      createDirectoryIfMissing )
import System.FilePath ( (</>) )
import System.Process ( readProcessWithExitCode )

sign :: FilePath -> IO ()
sign filePath =
  do tempDir <- getTemporaryDirectory
     uuid <- nextRandom
     let workDir = tempDir </> toString uuid
     createDirectoryIfMissing True workDir
     (_code,_out,_err) <-
       readProcessWithExitCode "tar"
                               ["xf",filePath,"-C",workDir,"--strip","1"]
                               mempty
     cabalFiles <-
       (filter (isSuffixOf ".cabal")) <$>
       (getDirectoryContents workDir)
     if length cabalFiles < 1
        then undefined
        else do pkg <-
                  cabalFilePackageId (workDir </> head cabalFiles)
                sig@(Signature signature) <- GPG.sign filePath
                let (PackageName name) = pkgName pkg
                    version =
                      showVersion (pkgVersion pkg)
                fingerprint <-
                  GPG.fingerprintFromVerify sig filePath
                req <-
                  parseUrl ("http://52.5.250.180:3000/upload/signature/" <> name <>
                            "/" <> version <> "/" <>
                            T.unpack (fingerprintSample fingerprint))
                let put =
                      req {method = methodPut
                          ,requestBody =
                             RequestBodyBS signature}
                res <- withManager (httpLbs put)
                if responseStatus res /= status200
                   then throwIO (GPGSignException "unable to sign & upload package")
                   else return ()

-- DO WE HAVE A PERMANENT URL OR DO WE NEED A FLEXIBLE RUN-TIME
-- CONFIG.YAML SERVER SETTING?
