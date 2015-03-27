{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Types for the signing process.

module Sig.Types
  (Mapping(..)
  ,Signer(..)
  ,FingerprintSample(..))
  where

import           Data.Aeson
import           Data.Char
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Distribution.Compat.ReadP
import           Distribution.Package (PackageName)
import           Distribution.ParseUtils
import           Text.Email.Validate

-- | A mapping from
data Mapping =
  Mapping {mappingSigners :: Map Signer (Set PackageName)}
  deriving (Show)

instance FromJSON Mapping where
  parseJSON j =
    do o <- parseJSON j
       signers <- o .: "signers"
       return (Mapping {mappingSigners = signers})

instance FromJSON (Map Signer (Set PackageName)) where
  parseJSON j =
    do os <- parseJSON j
       fmap M.fromList (mapM (\o -> do fingerprint <- o .: "fingerprint"
                                       email <- fmap unAeson (o .: "email")
                                       packages <- fmap (S.map unAeson) (o .: "packages")
                                       return (Signer fingerprint email,packages))
                             os)

-- | A package signer.
data Signer =
  Signer {signerFingerprint :: !FingerprintSample
         ,signerEmail :: !EmailAddress}
  deriving (Eq,Ord,Show)

-- | Handy wrapper for orphan instances.
newtype Aeson a =
  Aeson {unAeson :: a}
  deriving (Ord,Eq)

-- | The last few digits of a GPG fingerprint.
newtype FingerprintSample =
  FingerprintSample Text
  deriving (Eq,Ord,Show)

instance FromJSON FingerprintSample where
  parseJSON j =
    do s <- parseJSON j
       if T.all (\c -> isAlpha c || isDigit c) s
          then return (FingerprintSample s)
          else fail ("Expected finger print sample (alphanumeric), but got: " ++
                     T.unpack s)

instance FromJSON (Aeson PackageName) where
  parseJSON j =
    do s <- parseJSON j
       case readP_to_S parsePackageNameQ s of
         [(name,"")] -> return (Aeson name)
         _ -> fail ("Invalid package name: " ++ s)

instance FromJSON (Aeson EmailAddress) where
  parseJSON j =
    do s <- parseJSON j
       case validate (T.encodeUtf8 s) of
         Left e -> fail e
         Right k -> return (Aeson k)
