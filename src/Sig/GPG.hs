{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Sig.GPG
Description : Bulk Haskell Package Signing Tool: GPG
Copyright   : (c) FPComplete.com, 2015
License     : BSD3
Maintainer  : Tim Dysinger <tim@fpcomplete.com>
Stability   : experimental
Portability : POSIX
-}

module Sig.GPG where

import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString.Char8 as C
import Data.List (find)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Stack.Types.Sig
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)

sign
    :: (Monad m, MonadIO m, MonadThrow m)
    => FilePath -> m Signature
sign path = do
    (code,out,err) <-
        liftIO
            (readProcessWithExitCode
                 "gpg"
                 [ "--output"
                 , "-"
                 , "--use-agent"
                 , "--detach-sig"
                 , "--armor"
                 , path]
                 mempty)
    if code /= ExitSuccess
        then throwM (GPGSignException (out ++ "\n" ++ err))
        else return (Signature (C.pack out))

verifyFile
    :: (Monad m, MonadIO m, MonadThrow m)
    => Signature -> FilePath -> m Fingerprint
verifyFile (Signature signature) path =
    verifyFileWithProcess
        (readProcessWithExitCode
             "gpg"
             ["--verify", "-", path]
             (C.unpack signature))

verifyFileWithProcess
    :: (Monad m, MonadIO m, MonadThrow m)
    => IO (ExitCode, String, String) -> m Fingerprint
verifyFileWithProcess process = do
    (code,out,err) <- liftIO process
    if code /= ExitSuccess
        then throwM (GPGVerifyException (out ++ "\n" ++ err))
        else maybe
                 (throwM
                      (GPGFingerprintException
                           ("unable to extract short fingerprint from output\n: " <>
                            out)))
                 return
                 (let hasFingerprint =
                          (==) ["gpg:", "Signature", "made"] . take 3
                      fingerprint = T.pack . last
                  in Fingerprint . fingerprint <$>
                     find hasFingerprint (map words (lines err)))

fullFingerprint
    :: (Monad m, MonadIO m, MonadThrow m)
    => Fingerprint -> m Fingerprint
fullFingerprint (Fingerprint fp) = do
    (code,out,err) <-
        liftIO
            (readProcessWithExitCode "gpg" ["--fingerprint", T.unpack fp] [])
    if code /= ExitSuccess
        then throwM (GPGFingerprintException (out ++ "\n" ++ err))
        else maybe
                 (throwM
                      (GPGFingerprintException
                           ("unable to extract full fingerprint from output:\n " <>
                            out)))
                 return
                 (let hasFingerprint =
                          (==) ["Key", "fingerprint", "="] . take 3
                      fingerprint = T.pack . concat . drop 3
                  in Fingerprint . fingerprint <$>
                     find hasFingerprint (map words (lines out)))
