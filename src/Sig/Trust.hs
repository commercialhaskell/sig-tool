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
import Sig.Config
import Sig.GPG
import Sig.Types
import Text.Email.Validate (validate)

trust :: String -> String -> IO ()
trust fingerprint email =
  do cfg <- readConfig
     case validate (fromString email) of
       Left e ->
         throwIO (InvalidEmailException e)
       Right email' ->
         do fullFP <-
              fullFingerprint (FingerprintSample (fromString fingerprint))
            addSigner cfg (Signer fullFP email')
