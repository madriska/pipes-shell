{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverlappingInstances      #-}
{-# LANGUAGE TypeFamilies              #-}

-- | This module contains a few functions to use unix-y shell commands
-- as 'Pipe's.

module Pipes.Shell
  ( Cmd, Cmd'
  , CmdArg

  , runShell
  , (>?>)

  , cmdEnv,         cmd,         cmd'
  , pipeCmdEnv,     pipeCmd,     pipeCmd'
  , producerCmdEnv, producerCmd, producerCmd'
  , consumerCmdEnv, consumerCmd

  , ignoreErr, ignoreOut
  , markEnd
  ) where

import           Control.Applicative
import           Control.Arrow
import           Control.Monad
import           Data.Either
import           Pipes
import           Pipes.Core
import qualified Pipes.Prelude            as P
import           Pipes.Safe               hiding (handle)

import           Control.Concurrent       hiding (yield)
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import qualified System.IO                as IO
import           System.Process

import qualified Data.ByteString          as BS
import qualified Data.ByteString.Char8    as BSC
import qualified Data.Text                as T
import qualified Data.Text.IO             as TIO

-- | Like '>->' but marks the end of the left pipe with 'markEnd'.
-- It's needed because 'pipeCmdEnv' has to know when the upstream 'Pipe' finishes.
--
-- The basic rule is:
--
-- @ Replace every '>->' with '>?>' when it's in front of 'pipeCmdEnv' or similar. @
(>?>) :: Monad m =>
     Proxy a' a         () b m r ->
     Proxy () (Maybe b) c' c m r ->
     Proxy a' a         c' c m r
a >?> b = markEnd a >-> b
infixl 7 >?>

-- | Mark the end of a pipe.
-- It wraps all values in a 'Just' and yields *one* 'Nothing' after the upstream pipefinished.
markEnd :: Monad m =>
           Proxy a' a b' b         m r ->
           Proxy a' a b' (Maybe b) m r
markEnd pipe = do
  result <- for pipe (respond . Just)
  _ <- respond Nothing
  return result

-- | Ignore stderr from a 'pipeCmd'
ignoreErr :: (Monad m) => Pipe (Either BS.ByteString BS.ByteString) BS.ByteString m ()
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

linesFromHandle :: MonadIO m => IO.Handle -> Producer BS.ByteString m ()
linesFromHandle h = go
  where
  go = do
    eof <- liftIO $ IO.hIsEOF h
    if not eof then do
      line <- liftIO $ BSC.hGetLine h
      yield line
      go
    else
      liftIO $ IO.hClose h

fromTChan :: (MonadIO m) => TChan (Maybe a) -> Producer a m ()
fromTChan chan = do
  msg <- liftIO $ atomically $ readTChan chan
  whenJust msg $ \m -> do
    yield m
    fromTChan chan

toTChan :: MonadIO m => TChan a -> Producer a m () -> m ()
toTChan chan prod = runEffect $
  for prod (liftIO . atomically . writeTChan chan)

-- | An ad-hoc typeclass to get the varadic arguments and DWIM behavoir of 'cmdEnv'
class Cmd cmd where
  -- | Like 'pipeCmdEnv', 'producerCmdEnv' or 'consumerCmdEnv' depending on the context.
  -- It also supports varadic arguments.
  --
  -- Examples:
  --
  -- As 'Pipe':
  --
  -- >>> runShell $ yield "aaa" >?> cmdEnv Nothing "tr" ["a", "A"] >-> ignoreErr >-> P.stdoutLn
  -- AAA
  --
  -- As 'Producer':
  --
  -- >>> runShell $ cmdEnv Nothing "ls" >-> ignoreErr >-> P.stdoutLn
  -- <output from ls on the current directory>
  --
  -- As 'Consumer':
  --
  -- >>> runShell $ yield "aaa" >?> cmdEnv Nothing "cat > test.file"
  -- <a new file with "aaa" in it>


  cmdEnv :: CmdArg arg => Maybe [(String,String)] -> arg -> cmd

  -- | Like 'cmdEnv' but doesn't set enviorment varaibles
  cmd :: (Cmd cmd, CmdArg arg) => arg -> cmd
  cmd = cmdEnv Nothing

instance (Cmd cmd, CmdArg arg) => Cmd (arg -> cmd) where
  cmdEnv env' binary arg = cmdEnv env' $ toCmdStr binary ++ " " ++ toCmdStr arg

instance (MonadIO (Base m), MonadSafe m) =>
         Cmd (Pipe (Maybe BS.ByteString) (Either BS.ByteString BS.ByteString) m ()) where
  cmdEnv env' = pipeCmdEnv env' . toCmdStr

instance (MonadIO (Base m), MonadSafe m) =>
         Cmd (Producer (Either BS.ByteString BS.ByteString) m ()) where
  cmdEnv env' = producerCmdEnv env' . toCmdStr

instance (MonadIO (Base m), MonadSafe m) =>
         Cmd (Consumer (Maybe BS.ByteString) m ()) where
  cmdEnv env' = consumerCmdEnv env' . toCmdStr


-- | An ad-hoc typeclass to get the varadic arguments and DWIM behavoir of 'cmd''
-- This class is seperate from 'Cmd' to make the return types work out.
class Cmd' cmd where
  -- | Like 'pipeCmd'', 'producerCmd'' or 'consumerCmd' depending on context.
  -- It supports the same style of varadic arguments as 'cmd'
  cmd' :: CmdArg arg => arg -> cmd

instance (Cmd' cmd, CmdArg arg) => Cmd' (arg -> cmd) where
  cmd' binary arg = cmd' $ toCmdStr binary ++ " " ++ toCmdStr arg

instance (MonadIO (Base m), MonadSafe m) =>
         Cmd' (Pipe (Maybe BS.ByteString) BS.ByteString m ()) where
  cmd' = pipeCmd' . toCmdStr

instance (MonadIO (Base m), MonadSafe m) =>
         Cmd' (Producer BS.ByteString m ()) where
  cmd' = producerCmd' . toCmdStr

instance (MonadIO (Base m), MonadSafe m) =>
         Cmd' (Consumer (Maybe BS.ByteString) m ()) where
  cmd' = consumerCmd . toCmdStr

-- | This is the workhorse of this package.
--
-- It provides the direct interface from a shell command string to a proper
-- 'Pipe'.
--
-- >>> runShell $ yield (pack "aaa") >?> pipeCmdEnv Nothing "tr 'a' 'A'"
--            >-> P.map unpack >-> P.stdoutLn
-- AAA
--
-- It handles different string types for in and output.
pipeCmdEnv :: (MonadIO (Base m), MonadSafe m) =>
           Maybe [(String,String)] ->
           String ->
           Pipe (Maybe BS.ByteString) (Either BS.ByteString BS.ByteString) m ()
pipeCmdEnv env' cmd = bracket (aquirePipe env' cmd) releasePipe $
  \(stdin, stdout, stderr) -> do

    chan <- liftIO newTChanIO
    liftIO $ forkIO $
      handlesToChan stdout stderr chan

    body stdin chan

  where
  body stdin chan = do
    got <- await
    case got of
      Nothing -> do
        liftIO $ IO.hClose stdin
        yieldRest chan
      Just val -> do
        liftIO $ BS.hPutStr stdin val
        yieldOne chan
        body stdin chan

  -- TODO: think about joining yieldRest and yieldOne
  -- yield the whole chan until you're done
  yieldRest chan = do
    mLine <- liftIO $ atomically $ readTChan chan
    whenJust mLine $ \line -> do
      yield line
      yieldRest chan

  -- *try* to read one line from the chan and yield it.
  yieldOne chan = do
    isEmpty <- liftIO $ atomically $ isEmptyTChan chan
    unless isEmpty $ do
      mLine <- liftIO $ atomically $ readTChan chan
      whenJust mLine yield

  -- fill the TChan from the stdout and stderr handles
  -- the current implementation interleaves stderr and stdout
  handlesToChan stdout stderr chan = do
    out <- async $ toTChan chan $
           linesFromHandle stdout >-> P.map (Just . Right)

    err <- async $ toTChan chan $
           linesFromHandle stderr >-> P.map (Just . Left)

    forM_ [out, err] wait

    atomically $ writeTChan chan Nothing

-- | Like 'pipeCmdEnv' but doesn't set enviorment varaibles
pipeCmd :: (MonadIO (Base m), MonadSafe m) =>
           String ->
           Pipe (Maybe BS.ByteString) (Either BS.ByteString BS.ByteString) m ()
pipeCmd = pipeCmdEnv Nothing

-- | Like 'pipeCmd' but ignores stdout
pipeCmd' :: (MonadIO (Base m), MonadSafe m) =>
           String ->
           Pipe (Maybe BS.ByteString) BS.ByteString m ()
pipeCmd' cmd = pipeCmd cmd >-> ignoreErr

-- | Like 'pipeCmd' but closes the input end immediately.
--
-- Useful for command line tools like @ ls @
producerCmdEnv :: (MonadIO (Base m), MonadSafe m) =>
              Maybe [(String, String)] ->
              String ->
              Producer (Either BS.ByteString BS.ByteString) m ()
producerCmdEnv env' cmd = yield Nothing >-> pipeCmdEnv env' cmd

-- | Like 'producerCmdEnv' but doesn't set enviorment varaibles
producerCmd :: (MonadIO (Base m), MonadSafe m) =>
              String ->
              Producer (Either BS.ByteString BS.ByteString) m ()
producerCmd = producerCmdEnv Nothing

-- | Like 'producerCmd' but ignores stderr
producerCmd' :: (MonadIO (Base m), MonadSafe m) =>
              String ->
              Producer BS.ByteString m ()
producerCmd' cmd = producerCmd cmd >-> ignoreErr
-- | Like 'pipeCmd' but closes the output end immediately.
--
-- Useful for command line tools like @ cat > test.file @
consumerCmdEnv:: (MonadIO (Base m), MonadSafe m) =>
              Maybe [(String,String)] ->
              String ->
              Consumer (Maybe BS.ByteString) m ()
consumerCmdEnv env' cmd = pipeCmdEnv env' cmd >-> void await

-- | Like 'consumerCmdEnv' but doesn't set enviorment varaibles
consumerCmd:: (MonadIO (Base m), MonadSafe m) =>
              String ->
              Consumer (Maybe BS.ByteString) m ()
consumerCmd = consumerCmdEnv Nothing

-- | Creates the pipe handles
aquirePipe :: MonadIO m =>
              Maybe [(String, String)] ->
              String ->
              m (IO.Handle, IO.Handle, IO.Handle)
aquirePipe env' cmd = liftIO $ do
  (Just stdin, Just stdout, Just stderr, _) <- createProcess (shellPiped env' cmd)
  return (stdin, stdout, stderr)

-- | Releases the pipe handles
releasePipe :: MonadIO m => (IO.Handle, IO.Handle, IO.Handle) -> m ()
releasePipe (stdin, stdout, stderr) = liftIO $ do
  IO.hClose stdin
  IO.hClose stdout
  IO.hClose stderr

-- | Helper function to create the shell pipe handles
shellPiped :: Maybe [(String, String)] -> String -> CreateProcess
shellPiped env' cmd = CreateProcess
    { cmdspec = ShellCommand cmd
    , cwd          = Nothing
    , env          = env'
    , std_in       = CreatePipe
    , std_out      = CreatePipe
    , std_err      = CreatePipe
    , close_fds    = False
    , create_group = False
    }

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust (Just x) action = action x
whenJust Nothing _ = return ()

-- | An ad-hoc typeclass to make 'cmd' work with different string types
-- (in the command line arguments, not in the piped data)
-- This currently needs -XOverlappingInstaces,
-- but this could be solved the same way 'Show' 'String' is handled.
class CmdArg arg where
  toCmdStr :: arg -> String

instance CmdArg [Char] where
  toCmdStr = id

instance CmdArg T.Text where
  toCmdStr = T.unpack

instance CmdArg BSC.ByteString where
  toCmdStr = BSC.unpack

instance CmdArg arg => CmdArg [arg] where
  toCmdStr list = foldr sepBySpace "" strings
    where
    strings = map toCmdStr list
    sepBySpace acc x = acc ++ " " ++ x

-- | A simple run function for 'Pipe's that live in 'SafeT' 'IO'
runShell :: Effect (SafeT IO) r -> IO r
runShell = runSafeT . runEffect
