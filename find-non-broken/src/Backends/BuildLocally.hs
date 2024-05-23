{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}

module Backends.BuildLocally
  ( run
  ) where

import Types
import qualified Data.Text as T
import qualified GHC.IO as Unsafe
import qualified Control.Concurrent.MVar as MVar
import qualified Data.Time as Time
import qualified System.IO as IO
import qualified Data.Text.IO as TIO
import qualified UnliftIO.Async
import qualified System.Exit
import qualified Data.Text.Lazy.IO as LTIO
import qualified Data.Text.Lazy as LT
import qualified System.Process.Typed as Proc
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Lazy.Encoding as LTE
import qualified Control.Concurrent
import qualified System.IO.Temp as Tmp
import Control.Monad (unless, when)
import qualified Data.ByteString.Char8 as BSC8
import qualified System.Environment as Args

run
  :: NixPackageSet
  -> IO ()
run (NixPackageSet nixFile attr) = do
    drvPaths <- nixInstantiate nixFile attr
    buildDerivations 3600 drvPaths

nixInstantiate
  :: FilePath
  -> T.Text
  -> IO [DrvPath]
nixInstantiate nixFile attr = do
  putStrLn $ "Running command: " <> show cmd <> "..."
  (exitCode, stdOut, stdErr) <- Proc.readProcess cmd
  when (exitCode /= Proc.ExitSuccess) $
    fail $ "Command " <> show cmd <> " failed with " <> show exitCode <> ". Stderr:" <> BSC8.unpack (BSL.toStrict stdErr)
  let stdOutText = LTE.decodeUtf8 stdOut
  pure $ map (DrvPath . LT.toStrict) (LT.lines stdOutText)
  where
    cmd = Proc.proc "nix-instantiate" ["-A", T.unpack attr, nixFile]

{-# NOINLINE outputLock #-}
outputLock :: MVar.MVar ()
outputLock = Unsafe.unsafePerformIO $ MVar.newMVar ()

printStdout :: T.Text -> IO ()
printStdout str = do
  MVar.withMVar outputLock $ \() ->
    TIO.putStrLn str

-- | A Nix store path that ends in ".drv"
newtype DrvPath = DrvPath { unDrvPath :: T.Text }
  deriving (Eq, Show, Ord)

logMsg :: T.Text -> IO ()
logMsg str =
  MVar.withMVar outputLock $ \() ->
    TIO.hPutStrLn IO.stderr str

buildDerivations
  :: Time.DiffTime -- nix-build timeout
  -> [DrvPath]
  -> IO ()
buildDerivations timeoutSeconds drvPaths = do
  IO.hSetBuffering IO.stdout IO.LineBuffering -- make sure calling 'printStdout' outputs a line immediately
  numThreads <- Control.Concurrent.getNumCapabilities
  logMsg $ T.unwords
    [ "Building"
    , T.pack (show $ length drvPaths)
    , "derivations with"
    , T.pack (show numThreads)
    , "threads..."
    ]
  UnliftIO.Async.pooledMapConcurrently_ buildDrv drvPaths
  where
    buildDrv drvPath = do
      let drvPathText = unDrvPath drvPath
      logMsg $ "Building " <> drvPathText <> "..."
      (exitCode, output) <- runCommandInterleaved
        "nix-build"
        [ "--no-out-link"
        , "--timeout"
        , show (round timeoutSeconds :: Integer)
        , T.unpack drvPathText
        ]
      if exitCode == System.Exit.ExitSuccess
        then printStdout drvPathText
        else do
          logMsg $ "Building " <> drvPathText <> " failed."
          logMsg $ LT.toStrict $ LTE.decodeUtf8 output
          TIO.appendFile "failures.log" (drvPathText <> "\n")

runCommandInterleaved
  :: FilePath
  -> [String]
  -> IO (Proc.ExitCode, BSL.ByteString)
runCommandInterleaved cmd args =
  Proc.readProcessInterleaved $ Proc.proc cmd args

runCommandStream
  :: FilePath
  -> [String]
  -> IO Proc.ExitCode
runCommandStream cmd args = do
  Tmp.withSystemTempFile "stdout" $ \_ hStdout' ->
    Tmp.withSystemTempFile "stderr" $ \_ hStderr' -> do
      let stdoutStreamSpec = Proc.createPipe
          stderrStreamSpec = Proc.createPipe
          processConfig =
            Proc.setStdout stdoutStreamSpec $
              Proc.setStderr stderrStreamSpec $
                Proc.proc cmd args
      Proc.withProcessWait processConfig $ \process -> do
        let hStdout = Proc.getStdout process
            hStderr = Proc.getStderr process
        _ <- Control.Concurrent.forkIO $ readHandlePrintStderr hStdout
        _ <- Control.Concurrent.forkIO $ readHandlePrintStderr hStderr
        Proc.waitExitCode process
  where
    readHandlePrintStderr hdl =
      let go = do
            line <- TIO.hGetLine hdl
            logMsg line
            unless (T.null line)
              go
      in go

