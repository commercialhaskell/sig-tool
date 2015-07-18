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

keyExists :: forall (m :: * -> *).
             (Monad m,MonadIO m,MonadThrow m)
          => Signer -> m Bool
keyExists (Signer fingerprint _email) =
  fullFingerprint fingerprint >>
  -- FIXME this Bool is pointless because of exceptions
  return True

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

-- TODO always remove spaces from any FingerprintSample

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

-- verifyFileT :: forall (m :: * -> *).
--                (Monad m,MonadIO m)
--             => Signature -> FilePath -> MaybeT m FingerprintSample
-- verifyFileT (Signature signature) path =
--   verifyFileWithProcessT
--     (readProcessWithExitCode "gpg"
--                              ["--verify","-",path]
--                              (C.unpack signature))

-- verifyFileT' :: forall (m :: * -> *).
--                 (Monad m,MonadIO m)
--              => FilePath -> FilePath -> MaybeT m FingerprintSample
-- verifyFileT' signaturePath filePath =
--   verifyFileWithProcessT
--     (readProcessWithExitCode "gpg"
--                              ["--verify",signaturePath,filePath]
--                              [])

-- verifyFileWithProcessT :: forall (m :: * -> *) p.
--                           (Monad m,MonadIO m)
--                        => IO (ExitCode,String,String)
--                        -> MaybeT m FingerprintSample
-- verifyFileWithProcessT process =
--   do (code,_out,err) <- liftIO process
--      if code /= ExitSuccess
--         then fail err
--         else maybe (fail err)
--                    return
--                    (let hasFingerprint =
--                           (==) ["gpg:","Signature","made"] .
--                           take 3
--                         fingerprint = T.pack . last
--                     in FingerprintSample . fingerprint <$>
--                        find hasFingerprint (map words (lines err)))

-- shortFingerprintToLongT :: forall (m :: * -> *).
--                            (Monad m,MonadIO m)
--                         => FingerprintSample -> MaybeT m FingerprintSample
-- shortFingerprintToLongT (FingerprintSample fp) =
--   do (code,_out,err) <-
--        liftIO (readProcessWithExitCode "gpg"
--                                        ["--fingerprint",T.unpack fp]
--                                        [])
--      if code /= ExitSuccess
--         then fail err
--         else maybe (fail err)
--                    return
--                    (let hasFingerprint =
--                           (==) ["Key","fingerprint","="] .
--                           take 3
--                         fingerprint =
--                           T.pack .
--                           concat .
--                           drop 3
--                     in FingerprintSample . fingerprint <$>
--                        find hasFingerprint (map words (lines err)))

-- shortFingerprintFromVerify :: Signature -> FilePath -> IO (Maybe FingerprintSample)
-- shortFingerprintFromVerify (Signature signature) path =
--   do (code,_out,err) <-
--        readProcessWithExitCode "gpg"
--                                ["--verify","-",path]
--                                (C.unpack signature)
--      if code /= ExitSuccess
--         then throwIO (GPGVerifyException err)
--         else return (shortFingerprintFromVerifyOutput err)

-- shortFingerprintFromVerify' :: FilePath -> FilePath -> IO (Maybe FingerprintSample)
-- shortFingerprintFromVerify' signaturePath filePath =
--   do (code,_out,err) <-
--        readProcessWithExitCode "gpg"
--                                ["--verify",signaturePath,filePath]
--                                []
--      if code /= ExitSuccess
--         then throwIO (GPGVerifyException err)
--         else return (shortFingerprintFromVerifyOutput err)

-- longFingerprintFromShort :: FingerprintSample -> IO (Maybe FingerprintSample)
-- longFingerprintFromShort (FingerprintSample fp) =
--   do (code,_out,err) <-
--        readProcessWithExitCode "gpg"
--                                ["--fingerprint",T.unpack fp]
--                                []
--      if code /= ExitSuccess
--         then throwIO (GPGFingerprintException err)
--         else return (shortFingerprintFromVerifyOutput err)

-- {-| Find the short gpg signature from verify output.
-- Typical successful verify looks like the following:
-- @
-- gpg: assuming signed data in `../sig-archive/mappings/fpco.yaml'
-- gpg: Signature made Fri 17 Jul 2015 11:45:35 AM HST using RSA key ID 44A52A60
-- gpg: Good signature from "Tim Dysinger <tim@dysinger.net>"
-- gpg:                 aka "Tim Dysinger <tim@dysinger.org>"
-- gpg:                 aka "Tim Dysinger <dysinger@gmail.com>"
-- @
-- -}
-- shortFingerprintFromVerifyOutput :: String -> Maybe FingerprintSample
-- shortFingerprintFromVerifyOutput output =
--   let hasFingerprint =
--         (==) ["gpg:","Signature","made"] .
--         take 3
--       words' = map words . lines
--   in FingerprintSample . T.pack . last <$>
--      find hasFingerprint (words' output)

-- {-| Find the long gpg signature given a fingerprint.
-- Typical fingerprint output looks like the following:
-- @
-- pub   4096R/44A52A60 2010-11-04 [expires: 2015-11-03]
--       Key fingerprint = 8C69 4F5B 6941 3F16 736F  E055 A9E6 D147 44A5 2A60
-- uid                  Tim Dysinger <tim@dysinger.net>
-- uid                  Tim Dysinger <tim@dysinger.org>
-- uid                  Tim Dysinger <dysinger@gmail.com>
-- sub   4096R/XXXXXXXX 2010-11-04 [expires: 2015-11-03]
-- @
-- -}
-- longFingerprintFromFingerprintOutput :: String -> Maybe FingerprintSample
-- longFingerprintFromFingerprintOutput output =
--   let words' = map words . lines
--       hasFingerprint =
--         (==) ["Key","fingerprint","="] .
--         take 3
--       fingerprint =
--         T.pack .
--         concat .
--         drop 3
--   in FingerprintSample . fingerprint <$>
--      find hasFingerprint (words' output)
