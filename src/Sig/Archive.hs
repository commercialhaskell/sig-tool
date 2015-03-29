{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | Handling of archives. Writing, reading, etc.

module Sig.Archive where

import           Control.Arrow
import           Control.Monad
import qualified Data.ByteString as S (readFile)
import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Set (Set)
import qualified Data.Set as S
import           Distribution.Package
import           Sig.Cabal
import           Sig.Defaults
import           Sig.Mapping
import           Sig.Types
import           System.Directory.Parse
import           System.FilePath

-- | Read an archive from a directory.
readArchive :: FilePath -> IO Archive
readArchive dir =
  do mappingFilepaths <-
       filterDirectory (dir </> mappingsDir)
                       (isSuffixOf ".yaml")
     mappings <-
       mapM (\fp -> fmap (fp,) (readMapping fp)) mappingFilepaths
     signatures <-
       readSignatures (dir </> signaturesDir)
     return (Archive {archiveMappings = M.fromList mappings
                     ,archiveSignatures = signatures})

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
                                   fmap (PackageIdentifier name ver,)
                                        (forM signatures
                                              (\fp ->
                                                 fmap Signature (S.readFile fp))))
                             versions
                      return (map (second S.fromList) versionSignatures))
                packageNames)
