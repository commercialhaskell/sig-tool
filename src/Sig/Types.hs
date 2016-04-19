{-# LANGUAGE CPP #-}

{-|
Module      : Sig.Types
Description : Bulk Haskell Package Signing Tool: Types
Copyright   : (c) FPComplete.com, 2015
License     : BSD3
Maintainer  : Tim Dysinger <tim@fpcomplete.com>
Stability   : experimental
Portability : POSIX
-}

module Sig.Types (SigToolException(..), showException) where

#if __GLASGOW_HASKELL__ < 710
import Control.Exception (Exception)
#else
import Control.Exception (Exception, displayException)
#endif

data SigToolException
    = CabalFetchException String
    | CabalIndexException String
    | HackageAPIException String
    | ManifestParseException
    | DigestMismatchException
    deriving Show

showException :: SigToolException -> String
#if __GLASGOW_HASKELL__ < 710
showException = show
#else
showException = displayException
#endif

#if __GLASGOW_HASKELL__ < 710
instance Exception SigToolException where
#else
instance Exception SigToolException where
    displayException (CabalFetchException e) = e
    displayException (CabalIndexException e) = e
    displayException (HackageAPIException e) = e
    displayException ManifestParseException =
        "The manifest file could not be parsed"
    displayException DigestMismatchException =
        "Computed SHA digest for a package was wrong"
#endif
