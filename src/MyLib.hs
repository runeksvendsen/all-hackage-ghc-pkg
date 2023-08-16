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
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified System.IO.Temp as Tmp
import qualified System.Directory as Dir
import qualified System.Process as P
import qualified System.Exit
import qualified Codec.Binary.UTF8.String as UTF8
import qualified System.Environment
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text.Encoding.Error as T
import qualified Data.Text.Encoding as T
import System.FilePath ((</>))
import qualified System.IO as IO

-- TODO: hardcode all of these to a specific version
-- Available in this nix-shell:
--    NIX_PATH="nixpkgs=https://github.com/NixOS/nixpkgs/archive/release-21.05.tar.gz" nix-shell -p cabal-install -p cabal2nix -p nix
nix, cabal, cabal2nix :: FilePath
nix = "nix" -- version: 2.3.16
cabal = "cabal" -- version: 3.4.0.0
cabal2nix = "cabal2nix" -- version: 2.19.1

-- TODO: take as arg
cOMPILER_PATH :: FilePath
cOMPILER_PATH = "/nix/store/4gc0mizanhf1gbhfpkdmxasa0gl3jbak-ghc-9.6.2/bin/ghc"

main :: IO ()
main = do
  [drvFilePath] <- System.Environment.getArgs
  drvFileContent <- BS8.readFile drvFilePath
  let drvPathList = map (DrvPath . T.decodeUtf8With T.lenientDecode) $ BS8.lines drvFileContent
  main' drvPathList

withTmpDir
  :: String
  -> (FilePath -> IO a)
  -> IO a
withTmpDir name f =
  if not debug
    then Tmp.withSystemTempDirectory name f
    else do
      tmpBaseDir <- Tmp.getCanonicalTemporaryDirectory
      let dir = tmpBaseDir </> name
      Dir.createDirectory dir
      f dir
  where
    debug = False

main' :: [DrvPath] -> IO ()
main' drvPathList = withTmpDir "all-hackage-ghc-pkg" $ \tmpDir -> do
  IO.hPutStrLn IO.stderr $ "Using tmp dir: " <> tmpDir
  Dir.setCurrentDirectory tmpDir
  Right showDrv <- runCommand $ nixShowDerivation drvPathList
  let dependencies = map showDerivationToCabalDependency $ Map.elems showDrv
  Right () <- runCommand $ cabalInit cabalPackageName cOMPILER_PATH dependencies
  Right shellNix <- runCommand $ cabal2NixCreateShellNix tmpDir -- </> cabalPackageName <> ".cabal")
  BSL8.putStrLn shellNix
  where
    cabalPackageName = "all-hackage-deps"

-- | A Nix store path that ends in ".drv"
newtype DrvPath = DrvPath { unDrvPath :: T.Text }
  deriving (Eq, Show, Ord, FromJSON, ToJSON, FromJSONKey)

-- | TODO
data ShowDerivation = ShowDerivation
  { showDerivation_packageName :: T.Text -- env.pname
  , showDerivation_version :: T.Text -- env.version
  } deriving (Eq, Show, Ord)

-- E.g. "beam-core ==0.9.0.0"
showDerivationToCabalDependency
  :: ShowDerivation
  -> String
showDerivationToCabalDependency sd = T.unpack $
  T.unwords
    [ showDerivation_packageName sd
    , "==" <> showDerivation_version sd
    ]

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
  IO.hPutStrLn IO.stderr $ "Running " <> command_program cmd <> "..."
  (exitCode, stdout, stderr) <- P.readCreateProcessWithExitCode cp ""
  if exitCode == System.Exit.ExitSuccess
    then pure $ (command_outputToResult cmd) (BSL.pack $ UTF8.encode stderr) (BSL.pack $ UTF8.encode stdout)
    else error $
      unlines
        [ unwords
           [ "Command failed with"
           , show exitCode <> ":"
           , show cmd
           ]
        , stderr
        , stdout
        ]
  where
    cp = P.proc (command_program cmd) (command_arguments cmd)

nixShowDerivation
  :: [DrvPath]
  -> Command (Map.Map DrvPath ShowDerivation)
nixShowDerivation drvs =
  Command nix args convertResult
  where
    args =
      [ "show-derivation"
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
