{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Backends.GHActions
  ( run
  ) where

import Types
import qualified Data.Text as T
import qualified System.Process.Typed as Proc
import qualified Data.ByteString.Lazy as BSL
import Control.Monad (when)
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.Aeson
import qualified Data.Text.IO as TIO
import Data.String.Interpolate (i)
import qualified Data.Map as Map

run
  :: NixPackageSet
  -> IO ()
run nixPackageSet = do
    fileData <- genYaml nixPackageSet <$> nixEval nixPackageSet
    TIO.putStrLn fileData

nixEval
  :: NixPackageSet
  -> IO (Map.Map T.Text DrvPath)
nixEval (NixPackageSet nixFile attr) = do
  putStrLn $ "Running command: " <> show cmd <> "..."
  (exitCode, stdOut, stdErr) <- Proc.readProcess cmd
  when (exitCode /= Proc.ExitSuccess) $
    fail $ "Command " <> show cmd <> " failed with " <> show exitCode <> ". Stderr:" <> BSC8.unpack (BSL.toStrict stdErr)
  either
    (\e -> fail $ "BUG: Couldn't decode 'nix eval' output. Error: " <> e)
    pure
    (Data.Aeson.eitherDecode' stdOut)
  where
    cmd = Proc.proc "nix" ["eval", "-f", nixFile, T.unpack attr, "--json"]

-- | A Nix store path that ends in ".drv"
newtype DrvPath = DrvPath { unDrvPath :: T.Text }
  deriving (Eq, Show, Ord)

instance Data.Aeson.FromJSON DrvPath where
  parseJSON = Data.Aeson.withText "DrvPath" (pure . DrvPath)

genYaml
  :: NixPackageSet
  -> Map.Map T.Text DrvPath
  -> T.Text
genYaml nixPackageSet packages = do
  mkYaml "CI" nixPackageSet (Map.keys packages)

mkYaml
  :: T.Text
  -> NixPackageSet
  -> [T.Text]
  -> T.Text
mkYaml workflowName nixPackageSet packages = [i|
name: #{workflowName}

on:
  push:
  pull_request:

jobs:
  nix-build:
    strategy:
      matrix:
        nix_file: [#{nixPackageSetNixFile nixPackageSet}]
        package: #{"[" <> T.intercalate ", " packages <> "]"}
    permissions:
      contents: read
      id-token: write
    steps:
      - uses: actions/checkout@v3
      - uses: DeterminateSystems/nix-installer-action@main
      - uses: DeterminateSystems/magic-nix-cache-action@main
      - run: nix-build -f ${{ matrix.nix_file }} -A #{nixPackageSetAttribute nixPackageSet}.${{ matrix.package }}
|] :: T.Text
