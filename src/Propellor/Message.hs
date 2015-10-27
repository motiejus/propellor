-- | This module handles all display of output to the console when
-- propellor is ensuring Properties.
--
-- When two threads both try to display a message concurrently, 
-- the messages will be displayed sequentially.

module Propellor.Message (
	getMessageHandle,
	isConsole,
	forceConsole,
	actionMessage,
	actionMessageOn,
	warningMessage,
	infoMessage,
	errorMessage,
	stopPropellorMessage,
	processChainOutput,
	messagesDone,
	createProcessConcurrent,
	withConcurrentOutput,
) where

import System.Console.ANSI
import System.IO
import System.Log.Logger
import System.Log.Formatter
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple
import "mtl" Control.Monad.Reader
import Control.Applicative
import System.Directory
import Control.Monad.IfElse
import System.IO.Unsafe (unsafePerformIO)
import Control.Concurrent

import Propellor.Types
import Propellor.Types.Exception
import Utility.PartialPrelude
import Utility.Monad
import Utility.Exception

data MessageHandle = MessageHandle
	{ isConsole :: Bool
	}

-- | A shared global variable for the MessageHandle.
{-# NOINLINE globalMessageHandle #-}
globalMessageHandle :: MVar MessageHandle
globalMessageHandle = unsafePerformIO $ do
	c <- hIsTerminalDevice stdout
	newMVar $ MessageHandle c

getMessageHandle :: IO MessageHandle
getMessageHandle = readMVar globalMessageHandle

forceConsole :: IO ()
forceConsole = modifyMVar_ globalMessageHandle $ \mh ->
	pure (mh { isConsole = True })

whenConsole :: IO () -> IO ()
whenConsole a = whenM (isConsole <$> getMessageHandle) a

-- | Shows a message while performing an action, with a colored status
-- display.
actionMessage :: (MonadIO m, MonadMask m, ActionResult r) => Desc -> m r -> m r
actionMessage = actionMessage' Nothing

-- | Shows a message while performing an action on a specified host,
-- with a colored status display.
actionMessageOn :: (MonadIO m, MonadMask m, ActionResult r) => HostName -> Desc -> m r -> m r
actionMessageOn = actionMessage' . Just

actionMessage' :: (MonadIO m, ActionResult r) => Maybe HostName -> Desc -> m r -> m r
actionMessage' mhn desc a = do
	liftIO $ whenConsole $ do
		setTitle $ "propellor: " ++ desc
		hFlush stdout

	r <- a

	liftIO $ do
		whenConsole $
			setTitle "propellor: running"
		showhn mhn
		putStr $ desc ++ " ... "
		let (msg, intensity, color) = getActionResult r
		colorLine intensity color msg
		hFlush stdout

	return r
  where
	showhn Nothing = return ()
	showhn (Just hn) = do
		whenConsole $
			setSGR [SetColor Foreground Dull Cyan]
		putStr (hn ++ " ")
		whenConsole $
			setSGR []

warningMessage :: MonadIO m => String -> m ()
warningMessage s = liftIO $
	colorLine Vivid Magenta $ "** warning: " ++ s

infoMessage :: MonadIO m => [String] -> m ()
infoMessage ls = liftIO $ outputConcurrent $ concatMap (++ "\n") ls

-- | Displays the error message in red, and throws an exception.
--
-- When used inside a property, the exception will make the current
-- property fail. Propellor will continue to the next property.
errorMessage :: MonadIO m => String -> m a
errorMessage s = liftIO $ do
	colorLine Vivid Red $ "** error: " ++ s
	error "Cannot continue!"

colorLine :: ColorIntensity -> Color -> String -> IO ()
colorLine intensity color msg = do
	whenConsole $
		setSGR [SetColor Foreground intensity color]
	putStr msg
	whenConsole $
		setSGR []
	-- Note this comes after the color is reset, so that
	-- the color set and reset happen in the same line.
	, pure "\n"
	]

-- | Reads and displays each line from the Handle, except for the last line
-- which is a Result.
processChainOutput :: Handle -> IO Result
processChainOutput h = go Nothing
  where
	go lastline = do
		v <- catchMaybeIO (hGetLine h)
		case v of
			Nothing -> case lastline of
				Nothing -> do
					return FailedChange
				Just l -> case readish l of
					Just r -> pure r
					Nothing -> do
						outputConcurrent (l ++ "\n")
						return FailedChange
			Just s -> do
				outputConcurrent $
					maybe "" (\l -> if null l then "" else l ++ "\n") lastline
				go (Just s)

-- | Called when all messages about properties have been printed.
messagesDone :: IO ()
messagesDone = outputConcurrent
	=<< whenConsole (setTitleCode "propellor: done")
