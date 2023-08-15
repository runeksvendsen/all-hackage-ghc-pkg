{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module MyLib
  ( main
  ) where

import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Aeson as A
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified System.IO.Temp as Tmp
import qualified System.Directory as Dir
import qualified System.Process as P
import qualified System.Exit
import qualified Codec.Binary.UTF8.String as UTF8
import qualified System.Environment
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text.Encoding.Error as T
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.ByteString.Lazy as LT
import qualified Data.Text.Encoding as T

-- TODO: hardcode all of these to a specific version
nix = "nix" -- version: 2.11.1
cabal = "cabal" -- version: 3.4.0.0
cabal2nix = "cabal2nix" -- version: 2.19.1

main :: IO ()
main = do
  [drvFilePath] <- System.Environment.getArgs
  drvFileContent <- BS8.readFile drvFilePath
  let drvPathList = map (DrvPath . T.decodeUtf8With T.lenientDecode) $ BS8.lines drvFileContent
  main' drvPathList

main' :: [DrvPath] -> IO ()
main' drvPathList = Tmp.withSystemTempDirectory "create-cabal-project" $ \tmpDir -> do
  Dir.setCurrentDirectory tmpDir
  pure ()

-- | A Nix store path that ends in ".drv"
newtype DrvPath = DrvPath { unDrvPath :: T.Text }
  deriving (Eq, Show, Ord, FromJSON, ToJSON, FromJSONKey)

-- | TODO
data ShowDerivation = ShowDerivation
  { showDerivation_packageName :: T.Text -- env.pname
  , showDerivation_version :: T.Text -- env.version
  } deriving (Eq, Show, Ord)

instance FromJSON ShowDerivation where
  parseJSON = A.withObject "ShowDerivation" $ \o -> do
    env <- o .: "env"
    ShowDerivation
      <$> env .: "pname"
      <*> env .: "version"

-- |
data Command result = Command
  { command_program :: FilePath -- ^ The program to run, e.g. "cp"
  , command_arguments :: [String] -- The arguments to the program, e.g. ["srcDir", "dstDir"]
  , command_outputToResult :: BSL.ByteString -> BSL.ByteString -> Either String result -- ^ A function that converts the stderr and/or stdout to the result type
  }

instance Show (Command result) where
  show cmd = unwords
    [ "Command{"
    , "program=" <> show (command_program cmd)
    , "args=" <> show (command_arguments cmd)
    , "}"
    ]

-- TODO: make command_outputToResult consider exitCode somehow
runCommand :: Command result -> IO (Either String result)
runCommand cmd = do
  (exitCode, stdout, stderr) <- P.readCreateProcessWithExitCode cp ""
  if exitCode == System.Exit.ExitSuccess
    then pure $ (command_outputToResult cmd) (BSL.pack $ UTF8.encode stderr) (BSL.pack $ UTF8.encode stdout)
    else fail $ "Command failed with " <> show exitCode <> ":" <> show cmd
  where
    cp = P.proc (command_program cmd) (command_arguments cmd)

nixShowDerivation
  :: [DrvPath]
  -> Command (Map.Map DrvPath ShowDerivation)
nixShowDerivation drvs =
  Command nix args convertResult
  where
    args =
      [ "--extra-experimental-features", "nix-command"
      , "show-derivation"
      ] ++ map (T.unpack . unDrvPath) drvs

    convertResult _ =
      A.eitherDecode

cabalInit
  :: String
  -> FilePath
  -> [String]
  -> Command ()
cabalInit pkgName compilerPath dependencies =
  Command cabal args (\_ _ -> pure ())
  where
    args =
      [ "init"
      , "--non-interactive"
      , "--minimal"
      , "--package-name=" <> pkgName
      , "--lib"
      , "--license=BSD-3-Clause"
      , "--author=Blah"
      , "--email=blah@blah.com"
      , "--language=Haskell2010"
      , "--with-compiler=" <> compilerPath
      ] ++ concatMap (\dependency -> "-d" : dependency : []) dependencies

cabal2NixCreateShellNix
  :: String
  -> Command BSL.ByteString
cabal2NixCreateShellNix cabalProjectDir =
  Command cabal2nix ["--shell", cabalProjectDir] (\_ -> pure)
