{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}

module MyLib
  ( main
  ) where

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
import Control.Monad (unless)

main :: IO ()
main = do
  drvPaths <- readDrvPathsFromStdin
  buildDerivations 3600 drvPaths

readDrvPathsFromStdin :: IO [DrvPath]
readDrvPathsFromStdin = do
  stdinText <- LTIO.getContents
  pure $ map (DrvPath . LT.toStrict) (LT.lines stdinText)

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
      (exitCode, output) <- runCommand
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

runCommand
  :: FilePath
  -> [String]
  -> IO (Proc.ExitCode, BSL.ByteString)
runCommand cmd args =
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

