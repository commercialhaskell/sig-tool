{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Sig.Hackage where

import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson (eitherDecode)
import Data.Aeson.TH (deriveFromJSON, defaultOptions)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Data.Monoid ((<>))
import Distribution.Package (PackageIdentifier)
import Distribution.Text (simpleParse)
import Network.HTTP.Conduit
       (parseUrl, newManager, httpLbs, requestHeaders, responseBody,
        tlsManagerSettings)
import Sig.Types (SigException(HackageAPIException))

data UserDetail = UserDetail
    { groups :: [String]
    , username :: String
    , userid :: Integer
    } deriving (Show,Eq)

$(deriveFromJSON defaultOptions ''UserDetail)

packagesForMaintainer
    :: (MonadBaseControl IO m, MonadIO m, MonadThrow m)
    => String -> m [PackageIdentifier]
packagesForMaintainer uname = do
    mgr <- liftIO (newManager tlsManagerSettings)
    req <- parseUrl ("https://hackage.haskell.org/user/" <> uname)
    res <-
        liftIO
            (httpLbs
                 (req
                  { requestHeaders = [("Accept", "application/json")]
                  })
                 mgr)
    case (fmap packageNamesForUser <$> eitherDecode) (responseBody res) of
        Left err ->
            throwM
                (HackageAPIException
                     ("Cloudn't retrieve packages for user " <> uname <> ": " <>
                      show err))
        Right pkgs -> return pkgs

packageNamesForUser :: UserDetail -> [PackageIdentifier]
packageNamesForUser = mapMaybe packageNameFromGroup . groups
  where
    packageNameFromGroup grp =
        case filter ("" /=) (splitOn "/" grp) of
            ["package",pkg,"maintainers"] -> simpleParse pkg
            _ -> Nothing
