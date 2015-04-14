{-|
Module      : Sig.Cabal.Parse
Description : Haskell Package Signing Tool: PackageIdentifier Parsing Functions
Copyright   : (c) FPComplete.com, 2015
License     : BSD3
Maintainer  : Tim Dysinger <tim@fpcomplete.com>
Stability   : experimental
Portability : POSIX
-}

module Sig.Cabal.Parse where

import Data.Version ( Version )
import qualified Data.Version as V ( parseVersion )
import qualified Distribution.Compat.ReadP as Compat ( readP_to_S )
import Distribution.Package ( PackageName )
import Distribution.ParseUtils ( parsePackageNameQ )
import qualified Text.ParserCombinators.ReadP as ReadP
    ( readP_to_S )

-- | Parse a package name.
parsePackageName :: String -> Maybe PackageName
parsePackageName s =
  case reverse (Compat.readP_to_S parsePackageNameQ s) of
    ((name,""):_) -> Just name
    _ -> Nothing

-- FIXME This is returning Nothing for packages with a dash in the name (eg, auto-update)

-- | Parse a package version.
parseVersion :: String -> Maybe Version
parseVersion s =
  case reverse (ReadP.readP_to_S V.parseVersion s) of
    ((ver,""):_) -> Just ver
    _ -> Nothing
