{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Sig.Doc
Description : Haskell Package Signing Tool: Pretty Printing Functions
Copyright   : (c) FPComplete.com, 2015
License     : BSD3
Maintainer  : Tim Dysinger <tim@fpcomplete.com>
Stability   : experimental
Portability : POSIX
-}

module Sig.Doc
       (ToDoc(..), putToDoc, putCheckHeader, putPkgVerify,
        putInstallHeader)
       where

import BasePrelude hiding ((<+>),(<>),(<$>),empty)
import qualified Data.ByteString.Char8 as C ( unpack )
import Data.Map ( Map )
import qualified Data.Map.Strict as M ( toList )
import Data.Set ( Set )
import qualified Data.Set as S ( toList )
import Data.Text ( Text )
import qualified Data.Text as T ( unpack )
import Distribution.Package
    ( PackageName, PackageIdentifier(pkgName, pkgVersion) )
import Distribution.Text ( display )
import Sig.Types
    ( Config(..),
      FingerprintSample(FingerprintSample),
      Signer(..),
      Mapping(..),
      Signature,
      Archive(..) )
import Text.Email.Validate ( EmailAddress, toByteString )
import Text.PrettyPrint.ANSI.Leijen
    ( Doc,
      vsep,
      text,
      putDoc,
      linebreak,
      indent,
      hang,
      fill,
      empty,
      (<>),
      (<+>),
      (<$>) )

default (Text)

-- | Neatly print to a color ANSI console
class ToDoc a where
  toDoc :: a -> Doc

instance ToDoc Text where
  toDoc = text . T.unpack

instance ToDoc Config where
  toDoc Config{..} =
    text "Trusted Signers:" <$>
    hang 2 (indent 2 (vsep (map toDoc configTrustedMappingSigners))) <>
    linebreak

instance ToDoc Archive where
  toDoc Archive{..} =
    text "Mappings:" <$>
    hang 2 (indent 2 (toDoc archiveMappings)) <>
    text "Signatures:" <$>
    hang 2 (indent 2 (toDoc archiveSignatures))

instance ToDoc (Map Text Mapping) where
  toDoc x =
    vsep (map (\(a,b) ->
                 toDoc a <$>
                 hang 2 (indent 2 (toDoc b)))
              (M.toList x)) <>
    linebreak

instance ToDoc (Map PackageIdentifier (Set Signature)) where
  toDoc x =
    vsep (map (\(a,b) ->
                 toDoc a <$>
                 hang 2 (indent 2 (vsep (map toDoc (S.toList b)))))
              (M.toList x)) <>
    linebreak

instance ToDoc Mapping where
  toDoc Mapping{..} =
    vsep (map (\(a,b) ->
                 toDoc a <$>
                 hang 2 (indent 2 (vsep (map toDoc (S.toList b)))))
              (M.toList mappingSigners))

instance ToDoc Signer where
  toDoc Signer{..} =
    toDoc signerFingerprint <+>
    toDoc signerEmail

instance ToDoc Signature where
  toDoc = text . show

instance ToDoc EmailAddress where
  toDoc = text . C.unpack . toByteString

instance ToDoc FingerprintSample where
  toDoc (FingerprintSample x) = toDoc x

instance ToDoc PackageIdentifier where
  toDoc x =
    toDoc (pkgName x) <>
    text "-" <>
    text (intercalate "."
                      (map show (versionBranch $ pkgVersion x))) <>
    case versionTags (pkgVersion x) of
      [] -> empty
      xs ->
        text "-" <>
        text (intercalate "-" xs)

instance ToDoc PackageName where
  toDoc = text . display

putToDoc :: forall a. ToDoc a => a -> IO ()
putToDoc = putDoc . toDoc

putCheckHeader :: IO ()
putCheckHeader =
  putDoc (text "Checking" <>
          linebreak)

putPkgVerify :: PackageIdentifier -> IO ()
putPkgVerify pkg =
  putDoc (text "  " <> -- HACK - indent/hang/nest didn't work
          fill 74 (toDoc pkg) <>
          "OK" <>
          linebreak)

putInstallHeader :: IO ()
putInstallHeader =
  putDoc ("Installing" <> linebreak)
