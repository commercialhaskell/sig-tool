{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Sig.List
Description : Haskell Package Signing Tool: List Mappings
Copyright   : (c) FPComplete.com, 2015
License     : BSD3
Maintainer  : Tim Dysinger <tim@fpcomplete.com>
Stability   : experimental
Portability : POSIX
-}

module Sig.List where

import BasePrelude
import Sig.Archive ( readArchive )
import Sig.Config ( readConfig )
import Sig.Defaults ( configDir, archiveDir )
import Sig.Doc ( putToDoc )
import Sig.GPG ( verifyMappings )
import Sig.Types ( Archive(archiveMappings) )
import System.Directory ( getHomeDirectory )
import System.FilePath ( (</>) )

list :: IO ()
list =
  do cfg <- readConfig
     home <- getHomeDirectory
     let archDir = home </> configDir </> archiveDir
     arch <- readArchive archDir
     verifyMappings cfg
                    (archiveMappings arch)
                    archDir
     putToDoc (archiveMappings arch)
