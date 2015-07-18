{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Sig.Trust
Description : Haskell Package Signing Tool: Trusting Mappings by Key
Copyright   : (c) FPComplete.com, 2015
License     : BSD3
Maintainer  : Tim Dysinger <tim@fpcomplete.com>
Stability   : experimental
Portability : POSIX
-}

module Sig.Trust where

import BasePrelude
import Sig.Config ( readConfig, addSigner )
import Sig.GPG ( keyExists )
import Sig.Types
    ( SigException(GPGKeyMissingException, InvalidEmailException),
      FingerprintSample(FingerprintSample),
      Signer(Signer) )
import Text.Email.Validate ( validate )

trust :: String -> String -> IO ()
trust fingerprint email =
  do cfg <- readConfig
     -- FIXME: always throws an exception :|
     -- case eitherDecode (fromString fingerprint) of
     --   Left e ->
     --     throwIO (InvalidFingerprintException e)
     --   Right fingerprint' ->
     case validate (fromString email) of
       Left e ->
         throwIO (InvalidEmailException e)
       Right email' ->
         do let signer =
                  Signer (FingerprintSample (fromString fingerprint)) email'
            exists <- keyExists signer
            unless exists
                   (throwIO (GPGKeyMissingException
                               ("could not find key with fingerprint " <>
                                fingerprint)))
            addSigner cfg signer
