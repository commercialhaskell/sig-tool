{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Sig.Mapping
Description : Haskell Package Signing Tool: Signer Mapping Functions
Copyright   : (c) FPComplete.com, 2015
License     : BSD3
Maintainer  : Tim Dysinger <tim@fpcomplete.com>
Stability   : experimental
Portability : POSIX
-}

module Sig.Mapping where

import BasePrelude
import qualified Data.ByteString as S ( readFile )
import Sig.Types ( SigException(MappingParseException), Mapping )
import Data.Yaml ( decodeEither )

-- | Try to read a mapping from the file. Throws an exception if it
-- fails.
readMapping :: FilePath -> IO Mapping
readMapping fp =
  do mm <- S.readFile fp
     case decodeEither mm of
       Right m -> return m
       Left e ->
         throwIO (MappingParseException
                    ("Unable to parse mapping from mapping file " <> fp <> ": " <>
                     e))
