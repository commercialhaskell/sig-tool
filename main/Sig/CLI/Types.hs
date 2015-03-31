{-|
Module      : Sig.CLI.Types
Description : Haskell Package Signing Tool - CLI
Copyright   : (c) FPComplete.com, 2015
License     : BSD3
Maintainer  : Tim Dysinger <tim@fpcomplete.com>
Stability   : experimental
Portability : POSIX
-}

module Sig.CLI.Types where

data Options
  = Initialize
  | Update
  | Sign FilePath
  | List
  | Trust String
  | Check String
  | Install String
