{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Sig.GPG
Description : Haskell Package Signing Tool: GPG Packages
Copyright   : (c) FPComplete.com, 2015
License     : BSD3
Maintainer  : Tim Dysinger <tim@fpcomplete.com>
Stability   : experimental
Portability : POSIX
-}

module Sig.GPG where

import BasePrelude
import qualified Data.ByteString.Char8 as C ( unpack, pack )
import Data.Map.Strict as M ( lookup, Map, toList )
import qualified Data.Set as S ( toList, empty )
import Data.Text ( Text )
import qualified Data.Text as T ( unpack, pack )
import Distribution.Package
    ( PackageName(PackageName), PackageIdentifier(..), packageName )
import Sig.Defaults ( mappingsDir )
import Sig.Types
    ( SigException(GPGKeyMissingException, GPGNoSignatureException,
                   GPGSignException, GPGVerifyException),
      Config(Config),
      FingerprintSample(FingerprintSample),
      Signer(Signer),
      Mapping,
      Signature(..),
      Archive(archiveSignatures) )
import System.Directory ( doesFileExist )
import System.FilePath ( (</>) )
import System.Process ( readProcessWithExitCode )

sign :: FilePath -> IO Signature
sign path =
  do (code,out,err) <-
       readProcessWithExitCode "gpg"
                               ["--output","-","--detach-sig","--armor",path]
                               mempty
     if code /= ExitSuccess
        then throwIO (GPGSignException err)
        else return (Signature (C.pack out))

keyExists :: Signer -> IO Bool
keyExists (Signer (FingerprintSample fingerprint) _email) =
  do (code,_out,err) <-
       readProcessWithExitCode "gpg"
                               ["--fingerprint",T.unpack fingerprint]
                               mempty
     -- TODO verify fingerprint only brings back one key
     -- TODO verify email matches fingerprint
     if code /= ExitSuccess
        then throwIO (GPGKeyMissingException err)
        else return True

verifyPackage :: Archive -> PackageIdentifier -> FilePath -> IO ()
verifyPackage arch pkg@PackageIdentifier{..} path =
  let (PackageName name) = packageName pkg
  in case M.lookup pkg (archiveSignatures arch) of
       Nothing ->
         throwIO (GPGNoSignatureException ("no signature for package " <> name))
       Just sigs
         | S.empty == sigs ->
           throwIO (GPGNoSignatureException ("no signature for package " <> name))
       Just sigs ->
         forM_ (S.toList sigs)
               (\(Signature s) ->
                  do (code,_out,err) <-
                       readProcessWithExitCode "gpg"
                                               ["--verify","-",path]
                                               (C.unpack s)
                     if code /= ExitSuccess
                        then throwIO (GPGVerifyException err)
                        else return ())

verifyMappings :: Config
               -> (Map Text Mapping)
               -> FilePath
               -> IO ()
verifyMappings (Config signers) mappings dir =
  mapM_ (\(k,_v) ->
           verifyMapping (dir </> mappingsDir </> T.unpack k <> ".yaml"))
        (M.toList mappings)
  where verifyMapping filepath =
          do let signaturePath = filepath <> ".asc"
             exists <- doesFileExist signaturePath
             when (not exists)
                  (throwIO (GPGNoSignatureException signaturePath))
             (code,_out,err) <-
               readProcessWithExitCode "gpg"
                                       ["--verify",signaturePath,filepath]
                                       []
             if code /= ExitSuccess
                then throwIO (GPGVerifyException err)
                else do let fingerprint = fingerprintFromVerifyOutput err
                        when (not (any (\(Signer f _) -> f == fingerprint) signers))
                             (throwIO (GPGNoSignatureException
                                         ("no verifiable signature for " <> filepath)))

fingerprintFromVerify :: Signature -> FilePath -> IO FingerprintSample
fingerprintFromVerify (Signature signature) path =
  do (code,_out,err) <-
       readProcessWithExitCode "gpg"
                               ["--verify","-",path]
                               (C.unpack signature)
     if code /= ExitSuccess
        then throwIO (GPGVerifyException err)
        else return (fingerprintFromVerifyOutput err)

fingerprintFromVerifyOutput :: String -> FingerprintSample
fingerprintFromVerifyOutput =
  FingerprintSample .
  (T.pack . head . reverse . words . head . lines)
