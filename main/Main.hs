{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

{-|
Module      : Main
Description : Bulk Haskell Package Signing Tool: Main
Copyright   : (c) FPComplete.com, 2015
License     : BSD3
Maintainer  : Tim Dysinger <tim@fpcomplete.com>
Stability   : experimental
Portability : POSIX
-}

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>))
#endif


import Control.Exception (catch)
import Control.Monad.Logger (runLoggingT)
import qualified Data.ByteString.Char8 as BC
import Data.Monoid ((<>))
import Data.Time (getCurrentTime)
import Distribution.PackageDescription.TH
       (PackageDescription(package), PackageIdentifier(pkgVersion),
        packageVariable)
import Language.Haskell.TH (runIO, stringE)
import Options.Applicative
       (helper, execParser, subparser, str, progDesc, metavar, info,
        header, fullDesc, command, argument, strOption, long, short,
        showDefault, value)
import Sig.Tool
import Sig.Tool.Types
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr, stdout)
import System.Log.FastLogger (fromLogStr)

-- | Main entry point.
main :: IO ()
main = do
    let packageVersion = $(packageVariable (pkgVersion . package))
        buildDate = $(stringE =<< runIO (show `fmap` Data.Time.getCurrentTime))
        setupCmd =
            command
                "setup"
                (info
                     (helper <*> ((setup, ) <$> argument str (metavar "USER")))
                     (fullDesc <>
                      progDesc
                          "Download packages/ & create packages/manifest.yaml"))
        signCmd =
            command
                "sign"
                (info
                     (helper <*>
                      ((sign, ) <$>
                       strOption
                           (long "url" <> short 'u' <> metavar "URL" <>
                            showDefault <>
                            value "https://sig.commercialhaskell.org")))
                     (fullDesc <>
                      progDesc
                          "Sign packages/ listed in packages/manifest.yaml"))
        simpleLogger _loc _src _level msg =
            BC.hPutStrLn stdout (fromLogStr msg)
    (f,a) <-
        execParser
            (info
                 (helper <*> subparser (setupCmd <> signCmd))
                 (fullDesc <>
                  header ("sig " <> packageVersion <> " " <> buildDate) <>
                  progDesc "Haskell Package Bulk Signing Tool"))
    catch
        (runLoggingT (f a) simpleLogger)
        (\e ->
              do hPutStrLn stderr (showException (e :: SigToolException))
                 exitFailure)
