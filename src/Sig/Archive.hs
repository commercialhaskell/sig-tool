{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

{-|
Module      : Sig.Archive
Description : Haskell Package Signing Tool: Archive Functions
Copyright   : (c) FPComplete.com, 2015
License     : BSD3
Maintainer  : Tim Dysinger <tim@fpcomplete.com>
Stability   : experimental
Portability : POSIX
-}

module Sig.Archive where

import BasePrelude hiding (parseVersion)
import qualified Data.ByteString as S ( readFile )
import Data.List.Split ( splitOn )
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as M
    ( insert, fromList, empty, elems )
import Data.Set ( Set )
import qualified Data.Set as S ( fromList )
import qualified Data.Text as T ( stripSuffix, pack )
import Distribution.Package
    ( PackageIdentifier(PackageIdentifier) )
import Sig.Cabal.Parse ( parsePackageName, parseVersion )
import Sig.Defaults ( mappingsDir, signaturesDir )
import Sig.Mapping ( readMapping )
import Sig.Types ( Signature(..), Archive(..) )
import System.Directory.Parse ( parseDirectory, filterDirectory )
import System.FilePath ( splitFileName, splitExtension, (</>) )

-- | Read an archive from a directory.
readArchive :: FilePath -> IO Archive
readArchive dir =
  do mappingFilepaths <-
       parseDirectory
         (dir </> mappingsDir)
         (T.stripSuffix ".yaml" .
          T.pack)
     mappings <-
       mapM (\(fp,name) ->
               fmap (name,)
                    (readMapping fp))
            mappingFilepaths
     signatures <-
       readSignatures (dir </> signaturesDir)
     return Archive {archiveMappings = M.fromList mappings
                    ,archiveSignatures = signatures}

-- | Read all signatures from the directory.
readSignatures :: FilePath
               -> IO (Map PackageIdentifier (Set Signature))
readSignatures dir =
  do packageNames <-
       parseDirectory dir parsePackageName
     fmap (M.fromList . concat)
          (mapM (\(pkgDir,name) ->
                   do versions <-
                        parseDirectory pkgDir parseVersion
                      versionSignatures <-
                        mapM (\(verDir,ver) ->
                                do signatures <-
                                     filterDirectory verDir
                                                     (isSuffixOf ".asc")
                                   let signatures' =
                                         foldl (\a b ->
                                                  let sig =
                                                        snd (splitFileName (fst (splitExtension b)))
                                                  in case splitOn "_" sig of
                                                       (fprint:_date:_sha) ->
                                                         M.insert fprint b a
                                                       _ -> a)
                                               M.empty
                                               signatures
                                   fmap (PackageIdentifier name ver,)
                                        (forM (M.elems signatures')
                                              (fmap Signature .
                                               S.readFile)))
                             versions
                      return (map (second S.fromList) versionSignatures))
                packageNames)
