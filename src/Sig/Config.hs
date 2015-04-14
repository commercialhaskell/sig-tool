{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Sig.Config where

{-|
Module      : Sig.Config
Description : Haskell Package Signing Tool: Config File Functions
Copyright   : (c) FPComplete.com, 2015
License     : BSD3
Maintainer  : Tim Dysinger <tim@fpcomplete.com>
Stability   : experimental
Portability : POSIX
-}

import BasePrelude
import Data.Time ( formatTime, getCurrentTime )
import Sig.Defaults ( configDir, configFile )
import Sig.Types
    ( SigException(ConfigParseException), Config(..), Signer(..) )
import System.Directory
    ( renameFile,
      getHomeDirectory,
      doesFileExist,
      createDirectoryIfMissing )
import System.FilePath ( (</>) )
import System.Locale ( defaultTimeLocale )
import qualified Data.ByteString as B ( writeFile, readFile )
import qualified Data.Yaml as Y ( encode, decodeEither )
import Text.Email.Validate ( emailAddress )

defaultSigners :: [Signer]
defaultSigners =
  [Signer {signerFingerprint = "9616E227"
          ,signerEmail =
             fromJust (emailAddress "chrisdone@gmail.com")}
  ,Signer {signerFingerprint = "0D4F46E1"
          ,signerEmail =
             fromJust (emailAddress "michael@snoyman.com")}]

defaultConfig :: Config
defaultConfig = Config defaultSigners

readConfig :: IO Config
readConfig =
  do home <- getHomeDirectory
     let cfg = home </> configDir </> configFile
     result <- Y.decodeEither <$> B.readFile cfg
     case result of
       Left msg -> throwIO (ConfigParseException (show msg))
       Right config -> return config

writeConfig :: Config -> IO ()
writeConfig cfg =
  do home <- getHomeDirectory
     let configPath = home </> configDir </> configFile
     oldExists <- doesFileExist configPath
     when oldExists
          (do time <- getCurrentTime
              renameFile
                configPath
                (formatTime defaultTimeLocale
                            (configPath <> "-%s")
                            time))
     createDirectoryIfMissing True
                              (home </> configDir)
     B.writeFile
       configPath
       (Y.encode cfg {configTrustedMappingSigners =
                        nub (defaultSigners ++ configTrustedMappingSigners cfg)})

writeConfigIfMissing :: Config -> IO ()
writeConfigIfMissing cfg =
  do home <- getHomeDirectory
     let configPath = home </> configDir </> configFile
     fileExists <- doesFileExist configPath
     when (not fileExists)
          (writeConfig cfg)

addSigner :: Config -> Signer -> IO ()
addSigner cfg signer =
  writeConfig
    (cfg {configTrustedMappingSigners =
            (signer : configTrustedMappingSigners cfg)})
