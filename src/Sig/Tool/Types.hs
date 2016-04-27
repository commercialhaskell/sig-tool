{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE DeriveDataTypeable #-}
#endif

{-|
Module      : Sig.Tool.Types
Description : Bulk Haskell Package Signing Tool: Types
Copyright   : (c) FPComplete.com, 2015
License     : BSD3
Maintainer  : Tim Dysinger <tim@fpcomplete.com>
Stability   : experimental
Portability : POSIX
-}

module Sig.Tool.Types (SigToolException(..), showException) where

import Control.Exception (Exception)

#if __GLASGOW_HASKELL__ < 710
import Data.Data (Typeable)
#else
import Control.Exception (displayException)
#endif

data SigToolException
    = CabalFetchException String
    | CabalIndexException String
    | HackageAPIException String
    | ManifestParseException
    | DigestMismatchException
#if __GLASGOW_HASKELL__ < 710
    deriving (Show, Typeable)
#else
    deriving (Show)
#endif

showException :: SigToolException -> String
showException (CabalFetchException e) = e
showException (CabalIndexException e) = e
showException (HackageAPIException e) = e
showException ManifestParseException = "The manifest file could not be parsed"
showException DigestMismatchException = "Computed SHA digest for a package was wrong"

#if __GLASGOW_HASKELL__ < 710
instance Exception SigToolException
#else
instance Exception SigToolException where
    displayException = showException
#endif
