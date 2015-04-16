{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Sig.Update
Description : Haskell Package Signing Tool: Update Mappings & Signatures
Copyright   : (c) FPComplete.com, 2015
License     : BSD3
Maintainer  : Tim Dysinger <tim@fpcomplete.com>
Stability   : experimental
Portability : POSIX
-}

module Sig.Update where

import BasePrelude
import qualified Data.Conduit as C ( ($$+-) )
import Data.Conduit.Binary ( sinkFile )
import Data.Time ( formatTime, getCurrentTime )
import Network.HTTP.Conduit
    ( Response(responseBody), withManager, http, parseUrl )
import Sig.Defaults ( configDir, archiveDir )
import Sig.Types ( SigException(ArchiveUpdateException, SigServiceException) )
import System.Directory
    ( renameDirectory,
      getTemporaryDirectory,
      getHomeDirectory,
      doesDirectoryExist )
import System.FilePath ( (</>) )
import System.Locale ( defaultTimeLocale )
import System.Process ( readProcessWithExitCode )

update :: IO ()
update =
  do home <- getHomeDirectory
     temp <- getTemporaryDirectory
     let tempFile = temp </> "sig-archive.tar.gz"
         configPath = home </> configDir
         archivePath = configPath </> archiveDir
     request <-
       parseUrl "http://52.5.250.180:3000/download/archive"
     -- DO WE HAVE A PERMANENT URL THAT WE CAN PUT IN HERE OR DO WE
     -- NEED CONFIG PARAMS?
     catch
       (withManager
          (\mgr ->
             do res <- http request mgr
                (responseBody res) C.$$+-
                  sinkFile tempFile))
       (\e ->
          throwIO (SigServiceException (show (e :: SomeException))))
     oldExists <- doesDirectoryExist archivePath
     when oldExists
          (do time <- getCurrentTime
              renameDirectory
                archivePath
                (formatTime defaultTimeLocale
                            (archivePath <> "-%s")
                            time))
     (code,_out,err) <-
       readProcessWithExitCode "tar"
                               ["xf",tempFile,"-C",configPath]
                               mempty
     if code /= ExitSuccess
        then throwIO (ArchiveUpdateException err)
        else return ()
