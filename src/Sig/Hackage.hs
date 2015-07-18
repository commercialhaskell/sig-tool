{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Sig.Hackage where

import BasePrelude
import Control.Monad.Catch ( MonadThrow, throwM )
import Control.Monad.IO.Class ( MonadIO )
import Control.Monad.Trans.Control ( MonadBaseControl )
import Data.Aeson ( eitherDecode )
import Data.Aeson.TH ( deriveFromJSON, defaultOptions )
import Data.List.Split ( splitOn )
import Distribution.Package ( PackageIdentifier )
import Distribution.Text ( simpleParse )
import Network.HTTP.Conduit
    ( parseUrl, withManager, httpLbs, requestHeaders, responseBody )
import Sig.Types ( SigException(HackageAPIException) )

data UserDetail =
  UserDetail {groups :: [String]
             ,username :: String
             ,userid :: Integer}
  deriving (Show,Eq)

$(deriveFromJSON defaultOptions ''UserDetail)

packagesForMaintainer :: forall (m :: * -> *).
                         (MonadIO m,MonadThrow m,MonadBaseControl IO m)
                      => String -> m [PackageIdentifier]
packagesForMaintainer uname =
  do req <-
       parseUrl ("https://hackage.haskell.org/user/" <> uname)
     res <-
       withManager
         (httpLbs (req {requestHeaders =
                          [("Accept","application/json")]}))
     case (fmap packageNamesForUser <$> eitherDecode) (responseBody res) of
       Left err ->
         throwM (HackageAPIException
                   ("Cloudn't retrieve packages for user " <> uname <> ": " <>
                    show err))
       Right pkgs -> return pkgs

packageNamesForUser :: UserDetail -> [PackageIdentifier]
packageNamesForUser = mapMaybe packageNameFromGroup . groups
  where packageNameFromGroup grp =
          case filter ("" /=) (splitOn "/" grp) of
            ["package",pkg,"maintainers"] -> simpleParse pkg
            _ -> Nothing
