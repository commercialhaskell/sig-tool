{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
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

module Sig.Doc where

import BasePrelude hiding ((<>), empty)
import Control.Monad.Catch ()
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Control.Monad.Trans.Control ()
import Data.Text ( Text )
import qualified Data.Text as T ( unpack )
import Distribution.Package
    ( PackageName, PackageIdentifier(pkgName, pkgVersion) )
import Distribution.Text ( display )
import Text.PrettyPrint.ANSI.Leijen
    ( Doc,
      text,
      putDoc,
      linebreak,
      fill,
      empty,
      (<>) )

default (Text)

-- | Neatly print to a color ANSI console
class ToDoc a where
  toDoc :: a -> Doc

instance ToDoc Text where
  toDoc = text . T.unpack
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

putToDoc :: forall a (m :: * -> *).
            (ToDoc a,MonadIO m)
         => a -> m ()
putToDoc = liftIO . putDoc . toDoc

putHeader :: forall (m :: * -> *).
             MonadIO m
          => String -> m ()
putHeader a =
  liftIO (putDoc (text a <> linebreak))

putPkgOK :: forall (m :: * -> *).
            MonadIO m
         => PackageIdentifier -> m ()
putPkgOK pkg =
  liftIO (putDoc (text "  " <>
                  fill 74 (toDoc pkg) <>
                  "OK" <>
                  linebreak))
