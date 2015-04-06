-- | Missing Cabal functions.

module Sig.Cabal where

import           Data.Version (Version)
import qualified Data.Version as V
import qualified Distribution.Compat.ReadP as Compat
import           Distribution.Package
import           Distribution.ParseUtils
import qualified Text.ParserCombinators.ReadP as ReadP

-- | Parse a package name.
parsePackageName :: String -> Maybe PackageName
parsePackageName s =
  case reverse (Compat.readP_to_S parsePackageNameQ s) of
    ((name,""):_) -> Just name
    _ -> Nothing

-- | Parse a package version.
parseVersion :: String -> Maybe Version
parseVersion s =
  case reverse (ReadP.readP_to_S V.parseVersion s) of
    ((ver,""):_) -> Just ver
    _ -> Nothing
