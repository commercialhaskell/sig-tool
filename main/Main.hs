{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

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

import Control.Monad (join)
import Control.Exception (catch)
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
import Sig.Sign
import Sig.Types

-- | Main entry point.
main :: IO ()
main =
    let packageVersion = $(packageVariable (pkgVersion . package))
        buildDate = $(stringE =<< runIO (show `fmap` Data.Time.getCurrentTime))
        setupCmd =
            command
                "setup"
                (info
                     (helper <*> (setup <$> argument str (metavar "USER")))
                     (fullDesc <>
                      progDesc
                          "Download your packages & create a manifest.yaml"))
        signCmd =
            command
                "sign"
                (info
                     (helper <*>
                      (sign <$>
                       strOption
                           (long "url" <> short 'u' <> metavar "URL" <>
                            showDefault <>
                            value "https://sig.commercialhaskell.org")))
                     (fullDesc <> progDesc "Sign all your packages"))
    in catch
           (join
                (execParser
                     (info
                          (helper <*> subparser (setupCmd <> signCmd))
                          (fullDesc <>
                           header
                               ("sig " <> packageVersion <> " " <> buildDate) <>
                           progDesc "Haskell Package Bulk Signing Tool"))))
           (\e ->
                 putStrLn (showException (e :: SigToolException)))
