-- | Handling signer-package mappings.

module Sig.Mapping where

import qualified Data.ByteString as S
import           Sig.Types
import           Data.Yaml

-- | Try to read a mapping from the file. Throws an exception if it
-- fails.
readMapping :: FilePath -> IO Mapping
readMapping fp =
  do mm <- S.readFile fp
     case decodeEither mm of
       Right m -> return m
       Left e ->
         error ("Unable to parse mapping from mapping file " ++ fp ++ ": " ++ e)
