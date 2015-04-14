{-# LANGUAGE TupleSections #-}

{-|
Module      : System.Directory.Parse
Description : Haskell Package Signing Tool: Directory Parsing
Copyright   : (c) FPComplete.com, 2015
License     : BSD3
Maintainer  : Tim Dysinger <tim@fpcomplete.com>
Stability   : experimental
Portability : POSIX
-}

module System.Directory.Parse where

import Data.Maybe ( mapMaybe )
import System.Directory ( getDirectoryContents )
import System.FilePath ( (</>) )

-- | Parse a directory contents semantically.
parseDirectory :: FilePath            -- ^ The directory to look inside of.
               -> (String -> Maybe a) -- ^ A parse for each entry in the directory.
               -> IO [(FilePath,a)]   -- ^ A list of parsed paths and parse values.
parseDirectory dir parse =
  fmap (mapMaybe (\subdir ->
                    fmap (dir </> subdir,)
                         (parse subdir)))
       (getDirectoryContents dir)

-- | Filter a directory contents.
-- filterDirectory "/" (isPrefixOf "etc") -> ["/etc"]
filterDirectory :: FilePath         -- ^ The directory to look inside of.
                -> (String -> Bool) -- ^ A predicate for each entry in the directory.
                -> IO [FilePath]    -- ^ A list of filtered paths.
filterDirectory dir pred' =
  fmap (map fst)
       (parseDirectory
          dir
          (\fp ->
             if pred' fp
                then Just fp
                else Nothing))
