{-# LANGUAGE OverloadedStrings #-}

module MyLib
  ( main
  ) where

import Types
import qualified Backends.BuildLocally

import qualified Data.Text as T
import qualified System.Environment as Args

main :: IO ()
main = do
  args <- Args.getArgs
  case args of
    [command, nixFile, attr] -> do
      let nixPackageSet = NixPackageSet nixFile (T.pack attr)
      case command of
        "build" -> Backends.BuildLocally.run nixPackageSet
        other -> fail $ "Unrecognized command " <> show other <> ". Valid commands: build."
    _ -> fail "Expected three arguments: (1) command (2) .nix file (3) attribute name"
