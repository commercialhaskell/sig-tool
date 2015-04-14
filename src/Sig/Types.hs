{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Sig.Types
Description : Haskell Package Signing Tool: Types
Copyright   : (c) FPComplete.com, 2015
License     : BSD3
Maintainer  : Tim Dysinger <tim@fpcomplete.com>
Stability   : experimental
Portability : POSIX
-}

module Sig.Types
  (Archive(..)
  ,Signature(..)
  ,Mapping(..)
  ,Signer(..)
  ,FingerprintSample(..)
  ,Config(..)
  ,SigException(..))
  where

import Control.Exception ( Exception )
import Data.Aeson
    ( Value(String), ToJSON(..), FromJSON(..), object, (.=), (.:) )
import Data.ByteString ( ByteString )
import qualified Data.ByteString as SB ( take, length )
import Data.Char ( isDigit, isAlpha )
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as M ( fromList )
import Data.Set ( Set )
import qualified Data.Set as S ( map )
import Data.String ( IsString(..) )
import Data.Text ( Text )
import qualified Data.Text as T ( unpack, pack, all )
import qualified Data.Text.Encoding as T ( encodeUtf8, decodeUtf8 )
import Data.Typeable ( Typeable )
import Distribution.Package ( PackageName, PackageIdentifier )
import Sig.Cabal.Parse ( parsePackageName )
import Text.Email.Validate ( EmailAddress, validate, toByteString )

-- | A signature archive.
data Archive =
  Archive {archiveMappings :: !(Map Text Mapping)
          ,archiveSignatures :: !(Map PackageIdentifier (Set Signature))}
  deriving (Show)

-- | A signature.
newtype Signature = Signature ByteString
  deriving (Ord,Eq)

instance Show Signature where
  show (Signature s) =
    "Signature " ++
    (if SB.length s > 140
        then show (SB.take 140 s) ++
             "..."
        else show (SB.take 140 s))

-- | A mapping from signers to packages.
data Mapping =
  Mapping {mappingSigners :: !(Map Signer (Set PackageName))}
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

instance FromJSON Signer where
  parseJSON j =
    do o <- parseJSON j
       fingerprint <- o .: "fingerprint"
       email <- fmap unAeson (o .: "email")
       return (Signer fingerprint email)

instance ToJSON Signer where
  toJSON (Signer fingerprint email) =
    object ["fingerprint" .= fingerprint
           ,"email" .=
            toJSON (Aeson email)]

-- | Handy wrapper for orphan instances.
newtype Aeson a =
  Aeson {unAeson :: a}
  deriving (Ord,Eq)

-- | The last few digits of a GPG fingerprint.
newtype FingerprintSample =
  FingerprintSample {fingerprintSample :: Text}
  deriving (Eq,Ord,Show)

instance FromJSON FingerprintSample where
  parseJSON j =
    do s <- parseJSON j
       if T.all (\c -> isAlpha c || isDigit c) s
          then return (FingerprintSample s)
          else fail ("Expected finger print sample (alphanumeric), but got: " ++
                     T.unpack s)

instance ToJSON FingerprintSample where
  toJSON (FingerprintSample txt) = String txt

instance IsString FingerprintSample where
  fromString = FingerprintSample . T.pack

instance FromJSON (Aeson PackageName) where
  parseJSON j =
    do s <- parseJSON j
       case parsePackageName s of
         Just name -> return (Aeson name)
         Nothing ->
           fail ("Invalid package name: " ++ s)

instance FromJSON (Aeson EmailAddress) where
  parseJSON j =
    do s <- parseJSON j
       case validate (T.encodeUtf8 s) of
         Left e -> fail e
         Right k -> return (Aeson k)

instance ToJSON (Aeson EmailAddress) where
  toJSON (Aeson x) =
    String (T.decodeUtf8 (toByteString x))

-- | Config file ~/.sig/config.yaml
data Config =
  Config {configTrustedMappingSigners :: [Signer]}
  deriving Show

instance FromJSON Config where
  parseJSON j =
    do o <- parseJSON j
       signers <- o .: "trusted-mapping-signers"
       return (Config signers)

instance ToJSON Config where
  toJSON (Config signers) =
    object ["trusted-mapping-signers" .= signers]

-- | Exceptions
data SigException
  = ArchiveUpdateException { exMsg :: String }
  | CabalFetchException { exMsg :: String }
  | CabalInstallException { exMsg :: String }
  | CabalPackageListException { exMsg :: String }
  | ConfigParseException { exMsg :: String }
  | GPGKeyMissingException { exMsg :: String }
  | GPGNoSignatureException { exMsg :: String }
  | GPGSignException { exMsg :: String }
  | GPGVerifyException { exMsg :: String }
  | InvalidEmailException { exMsg :: String }
  | InvalidFingerprintException { exMsg :: String }
  | MappingParseException { exMsg :: String }
  | SigServiceException { exMsg :: String }
  deriving (Show,Typeable)

instance Exception SigException where
-- showMessage = exMsg -- ghc 7.10
