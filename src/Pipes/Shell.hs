{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE Rank2Types #-}

-- | This module contains a few functions to use unix-y shell commands
-- as 'Pipe's.
--
-- The output 'ByteString's from 'pipeCmdEnv' and friends are not line-wise,
-- but chunk-wise. To get proper lines
-- use the pipes-bytestring and the upcoming pipes-text machinery.
-- Note that exit code handling is not yet implemented.
--
-- All code examples in this module assume the following imports:
--
-- @
-- import Pipes.Prelude as P
-- import Pipes.ByteString as PBS
-- import Data.ByteString.Char8 as BSC
-- @

module Pipes.Shell
  (

  -- * Basic combinators
    pipeCmdEnv,     pipeCmd,     pipeCmd'
  , producerCmdEnv, producerCmd, producerCmd'
  , consumerCmdEnv, consumerCmd

  -- * Fancy overloads
  , Cmd, Cmd'
  , cmdEnv,         cmd,         cmd'

  -- * Utils
  , (>?>)
  , markEnd
  , ignoreErr, ignoreOut
  , runShell
  ) where

import           Control.Monad
import           Pipes
import qualified Pipes.ByteString               as PBS
import           Pipes.Core
import qualified Pipes.Prelude                  as P
import           Pipes.Safe                     hiding (handle)

import           Control.Concurrent             hiding (yield)
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TMChan
import qualified System.IO                      as IO
import           System.Process

import qualified Data.ByteString                as BS

-- Fancy overloads

-- | An ad-hoc typeclass to get the variadic arguments and DWIM behavior of 'cmdEnv'
class Cmd cmd where
  -- | Like 'pipeCmdEnv', 'producerCmdEnv' or 'consumerCmdEnv'
  -- depending on the context. It also supports variadic arguments.
  --
  -- Examples:
  --
  -- As 'Pipe':
  --
  -- >>> runShell $ yield (BSC.pack "aaa") >?> cmd "tr 'a' 'A'" >-> ignoreErr >-> PBS.stdout
  -- AAA
  --
  -- As 'Producer':
  --
  -- >>> runShell $ cmd "ls" >-> ignoreErr >-> PBS.stdout
  -- <output from ls on the current directory>
  --
  -- As 'Consumer':
  --
  -- >>> runShell $ yield (BSC.pack "aaa") >?> cmd "cat > test.file"
  -- <a new file with "aaa" in it>
  cmdEnv :: Maybe [(String,String)] -> String -> cmd

  -- | Like 'cmdEnv' but doesn't set environment variables
  cmd :: Cmd cmd => String -> cmd
  cmd = cmdEnv Nothing

instance Cmd cmd => Cmd (String -> cmd) where
  cmdEnv env' binary arg = cmdEnv env' $ binary ++ " " ++ arg

instance MonadSafe m =>
         Cmd (Pipe
              (Maybe BS.ByteString)
              (Either BS.ByteString BS.ByteString)
              m ()) where
  cmdEnv = pipeCmdEnv

instance MonadSafe m =>
         Cmd (Producer
              (Either BS.ByteString BS.ByteString)
              m ()) where
  cmdEnv = producerCmdEnv

instance MonadSafe m =>
         Cmd (Consumer
              (Maybe BS.ByteString)
              m ()) where
  cmdEnv = consumerCmdEnv


-- | An ad-hoc typeclass to get the variadic arguments and DWIM behavoir of 'cmd''.
-- This class is seperate from 'Cmd' to make the return types work out.
class Cmd' cmd where
  -- | Like 'cmd' but uses 'ignoreErr' automatically.
  -- So it's like 'pipeCmd'', 'producerCmd'' or 'consumerCmd' depending on context.
  -- It supports the same style of variadic arguments as 'cmd'
  cmd' :: String -> cmd

instance Cmd' cmd => Cmd' (String -> cmd) where
  cmd' binary arg = cmd' $ binary ++ " " ++ arg

instance MonadSafe m =>
         Cmd' (Pipe (Maybe BS.ByteString) BS.ByteString m ()) where
  cmd' = pipeCmd'

instance MonadSafe m =>
         Cmd' (Producer BS.ByteString m ()) where
  cmd' = producerCmd'

instance MonadSafe m =>
         Cmd' (Consumer (Maybe BS.ByteString) m ()) where
  cmd' = consumerCmd


-- Basic combinators

-- | This is the workhorse of this package.
--
-- It provides the direct interface from a shell command string to a proper
-- 'Pipe'.
--
-- >>> runShell $ yield (BSC.pack "aaa") >?> pipeCmdEnv Nothing "tr 'a' 'A'" >-> PBS.stdout
-- AAA
pipeCmdEnv :: MonadSafe m =>
    Maybe [(String,String)] ->
    String ->
    Pipe (Maybe BS.ByteString) (Either BS.ByteString BS.ByteString) m ()
pipeCmdEnv env' cmdStr = bracket (aquirePipe env' cmdStr) releasePipe $
  \(stdin, stdout, stderr) -> do

    chan <- liftIO newTMChanIO
    _ <- liftIO . forkIO $
      handlesToChan stdout stderr chan

    body stdin chan

  where
  body stdin chan = do
    got <- await
    case got of
      Nothing -> do
        liftIO $ IO.hClose stdin
        fromTMChan chan
      Just val -> do
        liftIO $ BS.hPutStr stdin val
        yieldOne chan
        body stdin chan

  -- *try* to read one line from the chan and yield it.
  yieldOne chan = do
    mLine <- liftIO $ atomically $ tryReadTMChan chan
    whenJust (join mLine)
      yield

  -- fill the TMChan from the stdout and stderr handles
  -- the current implementation reads stderr and stdout async
  handlesToChan stdout stderr chan = do
    out <- async $ toTMChan chan $ do
           PBS.fromHandle stdout >-> P.map Right
           liftIO $ IO.hClose stdout

    err <- async $ toTMChan chan $ do
           PBS.fromHandle stderr >-> P.map Left
           liftIO $ IO.hClose stderr

    forM_ [out, err] wait

    atomically $ closeTMChan chan

-- | Like 'pipeCmdEnv' but doesn't set environment variables
pipeCmd :: MonadSafe m =>
           String ->
           Pipe (Maybe BS.ByteString) (Either BS.ByteString BS.ByteString) m ()
pipeCmd = pipeCmdEnv Nothing

-- | Like 'pipeCmd' but ignores stderr
pipeCmd' :: MonadSafe m =>
           String ->
           Pipe (Maybe BS.ByteString) BS.ByteString m ()
pipeCmd' cmdStr = pipeCmd cmdStr >-> ignoreErr

-- | Like 'pipeCmdEnv' but closes the input end immediately.
--
-- Useful for command line tools like @ ls @
producerCmdEnv :: MonadSafe m =>
              Maybe [(String, String)] ->
              String ->
              Producer (Either BS.ByteString BS.ByteString) m ()
producerCmdEnv env' cmdStr = yield Nothing >-> pipeCmdEnv env' cmdStr

-- | Like 'producerCmdEnv' but doesn't set environment variables
producerCmd :: MonadSafe m =>
              String ->
              Producer (Either BS.ByteString BS.ByteString) m ()
producerCmd = producerCmdEnv Nothing

-- | Like 'producerCmd' but ignores stderr
producerCmd' :: MonadSafe m =>
              String ->
              Producer BS.ByteString m ()
producerCmd' cmdStr = producerCmd cmdStr >-> ignoreErr

-- | Like 'pipeCmd' but closes the output end immediately.
--
-- Useful for command line tools like @ cat > test.file @
consumerCmdEnv :: MonadSafe m =>
              Maybe [(String,String)] ->
              String ->
              Consumer (Maybe BS.ByteString) m ()
consumerCmdEnv env' cmdStr = pipeCmdEnv env' cmdStr >-> void await

-- | Like 'consumerCmdEnv' but doesn't set environment variables
consumerCmd :: MonadSafe m =>
              String ->
              Consumer (Maybe BS.ByteString) m ()
consumerCmd = consumerCmdEnv Nothing

-- Utils

-- | Like '>->' but marks the end of the left pipe with 'markEnd'.
-- It's needed because 'pipeCmdEnv' has to know when
-- the upstream 'Pipe' finishes.
--
-- The basic rule is:
--
-- @ Replace every '>->' with '>?>' when it's in front of
--   'pipeCmdEnv' or similar. @
(>?>) :: Monad m =>
     Proxy a' a         () b m r ->
     Proxy () (Maybe b) c' c m r ->
     Proxy a' a         c' c m r
a >?> b = markEnd a >-> b
infixl 7 >?>

-- | Mark the end of a pipe.
-- It wraps all values in a 'Just' and yields __one__ 'Nothing'
-- after the upstream pipe finished.
markEnd :: Monad m =>
           Proxy a' a b' b         m r ->
           Proxy a' a b' (Maybe b) m r
markEnd pipe = do
  result <- for pipe (respond . Just)
  _ <- respond Nothing
  return result

-- | Ignore stderr from a 'pipeCmd'
ignoreErr :: (Monad m) =>
             Pipe (Either BS.ByteString BS.ByteString) BS.ByteString m ()
ignoreErr = forever $ do
  val <- await
  case val of
    Left _ -> return ()
    Right x -> yield x

-- | Ignore stdout from a 'pipeCmd'
ignoreOut :: (Monad m) => Pipe (Either BS.ByteString BS.ByteString) BS.ByteString m ()
ignoreOut = forever $ do
  val <- await
  case val of
    Left x -> yield x
    Right _ -> return ()

-- | A simple run function for 'Pipe's that live in 'SafeT' 'IO'
runShell :: Effect (SafeT IO) r -> IO r
runShell = runSafeT . runEffect



-- Implementation utils

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust (Just x) action = action x
whenJust Nothing _       = return ()

fromTMChan :: (MonadIO m) => TMChan a -> Producer' a m ()
fromTMChan chan = do
  msg <- liftIO $ atomically $ readTMChan chan
  whenJust msg $ \m -> do
    yield m
    fromTMChan chan

toTMChan :: MonadIO m => TMChan a -> Producer' a m () -> m ()
toTMChan chan prod = runEffect $
  for prod (liftIO . atomically . writeTMChan chan)

-- | Creates the pipe handles
aquirePipe :: MonadIO m =>
              Maybe [(String, String)] ->
              String ->
              m (IO.Handle, IO.Handle, IO.Handle)
aquirePipe env' cmdStr = liftIO $ do
  (Just stdin, Just stdout, Just stderr, _) <-
    createProcess (shellPiped env' cmdStr)
  return (stdin, stdout, stderr)

-- | Releases the pipe handles
releasePipe :: MonadIO m => (IO.Handle, IO.Handle, IO.Handle) -> m ()
releasePipe (stdin, stdout, stderr) = liftIO $ do
  IO.hClose stdin
  IO.hClose stdout
  IO.hClose stderr

-- | Helper function to create the shell pipe handles
shellPiped :: Maybe [(String, String)] -> String -> CreateProcess
shellPiped env' cmdStr = CreateProcess
    { cmdspec = ShellCommand cmdStr
    , cwd          = Nothing
    , env          = env'
    , std_in       = CreatePipe
    , std_out      = CreatePipe
    , std_err      = CreatePipe
    , close_fds    = False
    , create_group = False
    , delegate_ctlc = False
    , detach_console = False
    , create_new_console = False
    , new_session = False
    , child_group = Nothing
    , child_user = Nothing
    , use_process_jobs = False
    }
