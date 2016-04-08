{-|
Module      : Sig.Types
Description : Bulk Haskell Package Signing Tool: Types
Copyright   : (c) FPComplete.com, 2015
License     : BSD3
Maintainer  : Tim Dysinger <tim@fpcomplete.com>
Stability   : experimental
Portability : POSIX
-}

module Sig.Types (SigException(..)) where

import Control.Exception (Exception)

data SigException
    = CabalFetchException String
    | CabalIndexException String
    | CabalInstallException String
    | CabalPackageListException String
    | GPGFingerprintException String
    | GPGKeyMissingException String
    | GPGNoSignatureException String
    | GPGSignException String
    | GPGVerifyException String
    | HackageAPIException String
    | SigServiceException String
    deriving (Show)

instance Exception SigException
