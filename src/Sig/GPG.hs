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
import qualified Data.Text as T ( pack )
import Sig.Types
    ( SigException(GPGSignException,
                   GPGVerifyException),
      FingerprintSample(FingerprintSample),
      Signature(..) )
import System.Process ( readProcessWithExitCode )

sign :: FilePath -> IO Signature
sign path =
  do (code,out,err) <-
       readProcessWithExitCode "gpg"
                               ["--output","-","--use-agent","--detach-sig","--armor",path]
                               mempty
     if code /= ExitSuccess
        then throwIO (GPGSignException err)
        else return (Signature (C.pack out))

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
