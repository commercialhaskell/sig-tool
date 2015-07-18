{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
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

import           BasePrelude
import           Control.Monad.Catch (MonadThrow, throwM)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString.Char8 as C
import           Data.Map (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import           Distribution.Package (PackageName(..), PackageIdentifier(..), packageName)
import           Sig.Defaults
import           Sig.Types
import           System.Directory (doesFileExist)
import           System.FilePath ((</>))
import           System.Process (readProcessWithExitCode)

sign :: forall (m :: * -> *).
        (Monad m,MonadIO m,MonadThrow m)
     => FilePath -> m Signature
sign path =
  do (code,out,err) <-
       liftIO (readProcessWithExitCode
                 "gpg"
                 ["--output","-","--use-agent","--detach-sig","--armor",path]
                 mempty)
     if code /= ExitSuccess
        then throwM (GPGSignException (out ++ "\n" ++ err))
        else return (Signature (C.pack out))

verifyPackage :: forall (m :: * -> *).
                  (Monad m,MonadIO m,MonadThrow m)
               => Archive -> PackageIdentifier -> FilePath -> m ()
verifyPackage arch pkg@PackageIdentifier{..} path =
  let (PackageName name) = packageName pkg
  in case M.lookup pkg (archiveSignatures arch) of
       Nothing ->
         throwM (GPGNoSignatureException ("no signature for package " <> name))
       Just sigs
         | S.empty == sigs ->
           throwM (GPGNoSignatureException ("no signature for package " <> name))
       Just sigs ->
         forM_ (S.toList sigs) (`verifyFile` path)

verifyMappings :: forall (m :: * -> *).
                  (Monad m,MonadIO m,MonadThrow m)
               => Config -> Map Text Mapping -> FilePath -> m ()
verifyMappings (Config signers) mappings dir =
  mapM_ (\(k,_v) ->
           verifyMapping (dir </> mappingsDir </> T.unpack k <> ".yaml"))
        (M.toList mappings)
  where verifyMapping filePath =
          do let signaturePath = filePath <> ".asc"
             exists <-
               liftIO (doesFileExist signaturePath)
             unless exists
                    (throwM (GPGNoSignatureException
                               ("signature file " <> signaturePath <>
                                " is missing")))
             fingerprint <-
               verifyFile' signaturePath filePath >>=
               fullFingerprint
             unless (any (\(Signer f _) -> f == fingerprint) signers)
                    (throwM (GPGNoSignatureException
                               ("no verifiable signature for " <> filePath)))

verifyFile :: forall (m :: * -> *).
              (Monad m,MonadIO m,MonadThrow m)
            => Signature -> FilePath -> m FingerprintSample
verifyFile (Signature signature) path =
  verifyFileWithProcess
    (readProcessWithExitCode "gpg"
                             ["--verify","-",path]
                             (C.unpack signature))

verifyFile' :: forall (m :: * -> *).
                (Monad m,MonadIO m,MonadThrow m)
             => FilePath -> FilePath -> m FingerprintSample
verifyFile' signaturePath filePath =
  verifyFileWithProcess
    (readProcessWithExitCode "gpg"
                             ["--verify",signaturePath,filePath]
                             [])

verifyFileWithProcess :: forall (m :: * -> *) p.
                         (Monad m,MonadIO m,MonadThrow m)
                      => IO (ExitCode,String,String) -> m FingerprintSample
verifyFileWithProcess process =
  do (code,out,err) <- liftIO process
     if code /= ExitSuccess
        then throwM (GPGVerifyException (out ++ "\n" ++ err))
        else maybe (throwM (GPGFingerprintException
                              ("unable to extract short fingerprint from output\n: " <>
                               out)))
                   return
                   (let hasFingerprint =
                          (==) ["gpg:","Signature","made"] .
                          take 3
                        fingerprint = T.pack . last
                    in FingerprintSample . fingerprint <$>
                       find hasFingerprint (map words (lines err)))

fullFingerprint :: forall (m :: * -> *).
                   (Monad m,MonadIO m,MonadThrow m)
                => FingerprintSample -> m FingerprintSample
fullFingerprint (FingerprintSample fp) =
  do (code,out,err) <-
       liftIO (readProcessWithExitCode "gpg"
                                       ["--fingerprint",T.unpack fp]
                                       [])
     if code /= ExitSuccess
        then throwM (GPGFingerprintException (out ++ "\n" ++ err))
        else maybe (throwM (GPGFingerprintException
                              ("unable to extract full fingerprint from output:\n " <>
                               out)))
                   return
                   (let hasFingerprint =
                          (==) ["Key","fingerprint","="] .
                          take 3
                        fingerprint =
                          T.pack .
                          concat .
                          drop 3
                    in FingerprintSample . fingerprint <$>
                       find hasFingerprint (map words (lines out)))
