-- |
-- Copyright:  (c) 2017 Ertugrul Söylemez
-- License:    BSD3
-- Maintainer: Ertugrul Söylemez <esz@posteo.de>
--
-- This module implements a progress bar with support for multiple
-- individual text chunks that can be updated independently (called
-- /meters/).

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module System.ProgressMeter
    ( -- * Tutorial
      -- $tutorial

      -- * Progress handles
      Progress,
      withProgress,
      hWithProgress,
      setProgressSep,
      -- ** Low-level
      newProgress,
      runProgress,
      quitProgress,

      -- * Meters
      Meter,
      setMeter,
      -- ** Creation and deletion
      appendMeter,
      deleteMeter,
      prependMeter,
      withAppendMeter,
      withPrependMeter,

      -- * Commands and messages
      putCmd,
      putMsg,
      putMsgLn
    )
    where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Foldable
import Data.IORef
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as Mi
import Data.List (intercalate)
import System.IO
import System.Mem.Weak


-- | Handle to an individual progress meter

data Meter =
    Meter {
      _meterGcVar   :: IORef (),
      _meterProg    :: Progress,
      _meterProgVar :: TVar String,
      _meterWeakRef :: Weak (IORef ())
    }


-- | Handle to a progress bar

data Progress =
    Progress {
      _progCmdVar    :: TQueue (Handle -> IO ()),
      _progProgsVar  :: TVar (IntMap (TVar String)),
      _progQuitVar   :: TVar Bool,
      _progSepVar    :: TVar String,
      _progSignalVar :: TVar Bool
    }


-- | Add a progress meter using the given key selection function

addMeterWith :: (forall a. IntMap a -> Int) -> Progress -> IO Meter
addMeterWith key _meterProg@Progress{..} =
    mask_ $ do
        (k, _meterProgVar) <- atomically $ do
            ps <- readTVar _progProgsVar
            let k = key ps
            progVar <- newTVar mempty
            writeTVar _progSignalVar True
            (k, progVar) <$ writeTVar _progProgsVar (Mi.insert k progVar ps)
        _meterGcVar <- newIORef ()
        _meterWeakRef <- mkWeakIORef _meterGcVar . atomically $
            modifyTVar _progProgsVar (Mi.delete k)
        pure Meter{..}


-- | Append a new progress meter to the given progress bar
--
-- The meter is removed when garbage-collected or when 'deleteMeter' is
-- used.  The latter is preferable.

appendMeter :: Progress -> IO Meter
appendMeter =
    addMeterWith (maybe 0 (succ . fst . fst) . Mi.maxViewWithKey)


-- | Delete the given progress meter
--
-- Changes to the meter after running this action will not have any
-- effect.

deleteMeter :: Meter -> IO ()
deleteMeter Meter{..} = do
    finalize _meterWeakRef
    atomically (writeTVar (_progSignalVar _meterProg) True)


-- | High-level interface to create a progress bar
--
-- This action creates a progress bar with the given update delay (in
-- microseconds) on the given output handle and runs it in a background
-- thread.  It passes the progress handle to the given function and
-- quits the bar after the action completes.

hWithProgress
    :: Int                 -- ^ Update delay (microseconds)
    -> Handle              -- ^ Output handle (most likely 'stderr')
    -> (Progress -> IO a)  -- ^ Action with progress bar
    -> IO a
hWithProgress delay h k = do
    prog <- newProgress
    withAsync (runProgress prog delay h) $ \a ->
        k prog `finally` do
            quitProgress prog
            waitCatch a


-- | Create a progress handle using the given update delay (in
-- microseconds)
--
-- Note: In most cases you can and should just use 'withProgress'.

newProgress :: IO Progress
newProgress = do
    _progCmdVar <- newTQueueIO
    _progProgsVar <- newTVarIO mempty
    _progQuitVar <- newTVarIO False
    _progSepVar <- newTVarIO " | "
    _progSignalVar <- newTVarIO False
    pure Progress{..}


-- | Prepend a new progress to the given progress bar
--
-- The meter is removed when garbage-collected or when 'deleteMeter' is
-- used.  The latter is preferable.

prependMeter :: Progress -> IO Meter
prependMeter =
    addMeterWith (maybe 0 (pred . fst . fst) . Mi.minViewWithKey)


-- | Send an action to be executed by the progress bar after temporarily
-- clearing its display
--
-- This function can be used, for example, to print something safely.
-- It returns immediately after queuing the action.  Commands are
-- executed in the order they are sent.
--
-- Actions sent by this function are /not/ subject to the update delay
-- and cause the display to be redrawn immediately.

putCmd
    :: Progress           -- ^ Progress bar
    -> (Handle -> IO ())  -- ^ Action to run, receives output handle
    -> IO ()
putCmd Progress{..} = atomically . writeTQueue _progCmdVar


-- | Send a message to be printed by the progress bar after temporarily
-- clearing its display
--
-- Messages are printed in the order they are sent.  Note: unless the
-- message includes a line feed, it will most likely be overwritten by
-- the progress bar.
--
-- Messages sent by this function are /not/ subject to the update delay
-- and cause the display to be redrawn immediately.

putMsg :: Progress -> String -> IO ()
putMsg prog str = putCmd prog (\h -> hPutStr h str)


-- | Variant of 'putMsg' that prints a line feed after the message

putMsgLn :: Progress -> String -> IO ()
putMsgLn prog str = putCmd prog (\h -> hPutStrLn h str)


-- | Make 'runProgress' clear its display and return
--
-- Note: In most cases you can and should just use 'withProgress'.

quitProgress :: Progress -> IO ()
quitProgress Progress{..} =
    atomically (writeTVar _progQuitVar True)


-- | Run the given progress bar
--
-- If the given handle is not a terminal, this action 
--
-- Note: In most cases you can and should just use 'withProgress'.

runProgress :: Progress -> Int -> Handle -> IO ()
runProgress Progress{..} delay h = do
    -- NOTE: Terminal width handling is disabled until the wcwidth()
    -- function is integrated in some way

    -- widthVar <- newTVarIO 80

    -- let updateTermWidth = do
    --         runInBoundThread $ do
    --             term <- setupTermFromEnv
    --             maybe (pure ())
    --                   (\w -> atomically (writeTVar widthVar w))
    --                   (getCapability term (tiGetNum "cols"))
    --         stopDelay prog
    --         atomically (writeTVar _progSignalVar True)

    -- when isTerm $ Codensity $ \k ->
    --     bracket
    --         (installHandler sigWINCH (Catch updateTermWidth) Nothing)
    --         (\old -> installHandler sigWINCH old Nothing)
    --         (\_ -> k ())

    isTerm <- hIsTerminalDevice h

    let go delayA =
            join . atomically $
                if isTerm
                  then command <|> quit <|> redrawNow
                  else commandNoTerm <|> quit

            where
            command = do
                c <- readTQueue _progCmdVar
                writeTVar _progSignalVar True
                pure $ do
                    cancel delayA
                    hPutStr h "\r\027[2K"
                    hFlush h
                    c h `catch` \(SomeException ex) -> do
                        hPrint h ex
                        hFlush h
                    go delayA

            commandNoTerm = do
                c <- readTQueue _progCmdVar
                pure $ do
                    c h `catch` \(SomeException ex) -> do
                        hPrint h ex
                        hFlush h
                    go delayA

            quit = do
                readTVar _progQuitVar >>= check
                pure (cancel delayA)

            redrawNow = do
                readTVar _progSignalVar >>= check
                waitCatchSTM delayA
                writeTVar _progSignalVar False
                sep <- readTVar _progSepVar
                str <- readTVar _progProgsVar >>=
                       fmap (intercalate sep . toList) . traverse readTVar
                pure $ do
                    hPutChar h '\r'
                    hPutStr h str
                    hPutStr h "\027[K"
                    hFlush h
                    async (threadDelay delay) >>= go

    (async (pure ()) >>= go) `finally`
        when isTerm (hPutStr h "\r\027[2K" >> hFlush h)


-- | Set the text of the given meter

setMeter :: Meter -> String -> IO ()
setMeter Meter{..} str = do
    readIORef _meterGcVar
    atomically $ do
        writeTVar _meterProgVar str
        writeTVar (_progSignalVar _meterProg) True


-- | Set the separator string between individual meters (@" | "@ by
-- default)

setProgressSep :: Progress -> String -> IO ()
setProgressSep Progress{..} sep =
    atomically $ do
        writeTVar _progSepVar sep
        writeTVar _progSignalVar True


-- | High-level interface to 'appendMeter' that makes sure the meter is
-- deleted after the given action

withAppendMeter :: Progress -> (Meter -> IO a) -> IO a
withAppendMeter prog =
    bracket (appendMeter prog)
            deleteMeter


-- | High-level interface to 'prependMeter' that makes sure the meter is
-- deleted after the given action

withPrependMeter :: Progress -> (Meter -> IO a) -> IO a
withPrependMeter prog =
    bracket (prependMeter prog)
            deleteMeter


-- | Variant of 'hWithProgress' that uses 'stderr'

withProgress
    :: Int                 -- ^ Update delay (microseconds)
    -> (Progress -> IO a)  -- ^ Action with progress bar
    -> IO a
withProgress delay = hWithProgress delay stderr


{- $tutorial

First you need to create a progress bar.  The easiest way is to use the
'withProgress' function:

> withProgress 100000 $ \prog -> do
>     -- stuff --

The first argument to the function is the update delay in microseconds.
Each time the bar display is updated, a timer of that duration is
started, during which no further updates are drawn.  When the action
given to 'withProgress' finishes, the display is cleared.

In order to actually draw something you need to create a 'Meter', which
corresponds to a dynamic-width space within the progress bar.  The
recommended interfaces to do that are 'withAppendMeter' and
'withPrependMeter'.  The function 'setMeter' sets the content of that
meter.  Example:

> import Control.Concurrent
> import System.ProgressMeter
>
> main =
>     withProgress 100000 $ \prog ->
>         withAppendMeter prog $ \meter -> do
>             setMeter meter "Hello ..."
>             threadDelay 1000000
>             setMeter meter "... world!"
>             threadDelay 1000000

In many applications you will want to print diagnostic messages that
should not be treated as part of the progress bar, but should just
scroll by as regular terminal text.  You can do that by using 'putCmd',
'putMsg' and 'putMsgLn':

> import Control.Concurrent
> import System.ProgressMeter
>
> main =
>     withProgress 100000 $ \prog ->
>         withAppendMeter prog $ \meter -> do
>             setMeter meter "Hello ..."
>             threadDelay 1000000
>             putMsgLn prog "Some diagnostics."
>             threadDelay 1000000
>             putMsgLn prog "Some more diagnostics."
>             threadDelay 1000000
>             setMeter meter "... world!"
>             threadDelay 1000000
>             putMsgLn prog "More and more diagnostics."
>             threadDelay 1000000

Of course the main purpose of this library is to show a progress bar for
concurrent activity.  Therefore meters can be created and updated from
separate threads.  Run the following program and watch how the
individual threads update their meters, print diagnostics and disappear
concurrently:

> import Control.Concurrent
> import Control.Concurrent.Async
> import Control.Monad
> import Data.Foldable
> import Text.Printf
>
> main =
>     withProgress 500000 $ \prog ->
>         let thread n = do
>                 threadDelay (100000*n)
>                 withAppendMeter prog $ \meter -> do
>                     putMsgLn prog (printf "Thread %d started." n)
>                     for_ [0..100 :: Int] $ \p -> do
>                         when (p == 50) $
>                             putMsgLn prog (printf "Thread %d reached half-way point." n)
>                         setMeter meter (printf "T%d: %d%%" n p)
>                         threadDelay (280000 - 40000*n)
>                     putMsgLn prog (printf "Thread %d done." n)
>                     threadDelay 500000
>         in mapConcurrently_ thread [1..6]

The actual terminal handling is very conservative.  Only ANSI codes are
used to draw the display, and terminal width is not taken into account
in this version.  If the output handle is not a terminal, the meters are
not drawn, but only messages sent by 'putMsg' and 'putMsgLn' are
printed.

-}
