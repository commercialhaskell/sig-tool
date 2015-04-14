{-|
Module      : Sig.Defaults
Description : Haskell Package Signing Tool: Defaults
Copyright   : (c) FPComplete.com, 2015
License     : BSD3
Maintainer  : Tim Dysinger <tim@fpcomplete.com>
Stability   : experimental
Portability : POSIX
-}

module Sig.Defaults where

-- | Place for the sig config
configDir :: FilePath
configDir = ".sig"

-- | Place for the config file.
configFile :: FilePath
configFile = "config.yaml"

-- | Place for the archive
archiveDir :: FilePath
archiveDir = "sig-archive"

-- | Place for mappings.
mappingsDir :: FilePath
mappingsDir = "mappings/"

-- | Place for signatures.
signaturesDir :: FilePath
signaturesDir = "signatures/"
