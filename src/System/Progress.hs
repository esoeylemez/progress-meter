-- |
-- Copyright:  (c) 2017 Ertugrul Söylemez
-- License:    BSD3
-- Maintainer: Ertugrul Söylemez <esz@posteo.de>
--
-- This module implements a progress bar with support for multiple
-- individual text chunks that can be updated independently (called
-- /meters/).

{-# LANGUAGE RankNTypes #-}

module System.Progress
    ( -- * Tutorial
      -- $tutorial

      -- ** Zooming
      -- $zooming

      -- ** Concurrent updates
      -- $concurrent_updates

      -- ** Further notes
      -- $notes

      -- * Progress bars
      Progress(..),
      withProgress,
      withProgress_,
      -- ** I/O
      meterIO,
      putMsg,
      putMsgLn,

      -- * Meters
      Meter,
      Meter',
      modifyMeter,
      setMeter,
      zoomMeter,
      zoomMeterL,
      -- ** STM variants
      modifyMeterSTM,
      setMeterSTM
    )
    where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Functor.Identity
import System.Console.ANSI
import System.IO


-- | Progress bars displaying state information of the given type

data Progress a =
    Progress {
      progressDelay   :: Int,         -- ^ Update delay in microseconds
      progressHandle  :: Handle,      -- ^ Output handle
      progressInitial :: a,           -- ^ Initial state
      progressRender  :: a -> String  -- ^ State renderer
    }


-- | A value of type @Meter a b@ can be used to update part of the
-- current state of the progress bar by supplying a function of type @(a
-- -> b)@, where @a@ is the type of the current value and @b@ is the
-- type of the new value.  See the 'modifyMeter' function for details.
--
-- In most cases you can just assume @a = b@ and use the @Meter'@ alias.

data Meter a b =
    Meter {
      _mModify :: (a -> b) -> STM (),
      _mRunIO  :: forall r. (Handle -> IO r) -> IO r
    }


-- | Handy type alias for the common case where the current state type
-- and the new state type are the same

type Meter' a = Meter a a


-- | Perform the given action while temporarily hiding the progress bar
--
-- The given action is sequenced with concurrent uses of @meterIO@, so
-- it can be used for regular output without artifacts.  The function
-- receives the output handle of the progress bar.

meterIO :: Meter a b -> (Handle -> IO r) -> IO r
meterIO = _mRunIO


-- | Modify the part of the state represented by the given meter using
-- the given function
--
-- The function receives the current value of type @a@ of the meter and
-- should return the new value of type @b@.  Note that for most
-- applications those types will be the same.
--
-- Updates are performed strictly, so they don't pile up when updates
-- are throttled, unless the progress bar is disabled (because the
-- output handle is not a terminal), in which case no state is
-- maintained at all.

modifyMeter :: Meter a b -> (a -> b) -> IO ()
modifyMeter meter = atomically . _mModify meter


-- | STM variant of 'modifyMeter': modify the given meter in a
-- transaction
--
-- You can use this function to modify multiple meters simultaneously.
-- This is useful, if you want to make sure that users don't observe
-- partial updates.

modifyMeterSTM :: Meter a b -> (a -> b) -> STM ()
modifyMeterSTM = _mModify


-- | Variant of 'putMsgLn' that omits the final line feed
--
-- Note: Use this function only when the given string ends with a line
-- feed, otherwise the progress bar will overwrite its last line when it
-- is redisplayed.

putMsg :: Meter a b -> String -> IO ()
putMsg meter str = meterIO meter (\h -> hPutStr h str)


-- | Print the given string to the output handle of the progress bar
--
-- This is implemented in terms of 'meterIO', so it does The Right
-- Thing: it temporarily hides the progress bar, prints the string, then
-- redisplays it.  It also makes sure that concurrent messages are
-- properly sequenced.

putMsgLn :: Meter a b -> String -> IO ()
putMsgLn meter str = meterIO meter (\h -> hPutStrLn h str)


-- | Variant of 'modifyMeter': set the given meter to the given new
-- state
--
-- See 'modifyMeter' for details.

setMeter :: Meter a b -> b -> IO ()
setMeter m = modifyMeter m . const


-- | Variant of 'modifyMeterSTM': set the given meter to the given new
-- state in a transaction
--
-- See 'modifyMeterSTM' for details.

setMeterSTM :: Meter a b -> b -> STM ()
setMeterSTM m = modifyMeterSTM m . const


-- | Display a progress bar for the duration of the given action
--
-- Note: If the output handle is not a terminal (as determined by
-- 'hIsTerminalDevice'), no progress bar is displayed and no state is
-- maintained.  In this case 'modifyMeter' and 'modifyMeterSTM' are
-- no-ops.
--
-- For most applications the simpler variant 'withProgress_' is
-- sufficient.

withProgress
    :: Progress a          -- ^ Progress bar configuration
    -> (Meter' a -> IO r)  -- ^ Action with a progress bar
    -> IO r
withProgress prog k = do
    let h  = progressHandle prog
        x0 = progressInitial prog

    stateVar <- newTVarIO (progressInitial prog)
    changeVar <- newTVarIO False
    drawLock <- newMVar ()

    let withDrawLock c =
            modifyMVar drawLock (\s -> (,) s <$> c)

        clear = do
            hPutChar h '\r'
            hClearFromCursorToLineEnd h
            hFlush h

        render x = do
            hPutChar h '\r'
            hPutStr h (progressRender prog x)
            hClearFromCursorToLineEnd h
            hFlush h

        renderLoop = (withDrawLock (render x0) >> go)
                     `finally` withDrawLock clear
            where
            go = join . atomically $ do
                readTVar changeVar >>= check
                writeTVar changeVar False
                x <- readTVar stateVar
                pure $ do
                    withDrawLock (render x)
                    threadDelay (progressDelay prog)
                    go

        mModify f = do
            x <- f <$> readTVar stateVar
            x `seq` writeTVar stateVar x
            writeTVar changeVar True

        mRunIO f = withDrawLock $ do
            clear
            f h `finally` (atomically (readTVar stateVar) >>= render)

        mRunIONoTerm f = withDrawLock (f h)

    isTerm <- hIsTerminalDevice h
    if isTerm
      then withAsync renderLoop $ \_ ->
               k (Meter { _mModify = mModify,
                          _mRunIO  = mRunIO })
      else k (Meter { _mModify = const (pure ()),
                      _mRunIO  = mRunIONoTerm })


-- | Simpler variant of 'withProgress'
--
-- Uses a delay of 0.1 seconds and displays the progress bar on stderr.

withProgress_
    :: a                   -- ^ Initial state value
    -> (a -> String)       -- ^ State renderer
    -> (Meter' a -> IO r)  -- ^ Action with a progress bar
    -> IO r
withProgress_ x0 render = withProgress prog
    where
    prog = Progress {
             progressDelay   = 100000,
             progressHandle  = stderr,
             progressInitial = x0,
             progressRender  = render
           }


-- | Zoom into part of the state
--
-- This function returns a variant of the given meter that focusses on
-- the value(s) the given setter modifies.  You can use this for example
-- to focus on a single key in a map or all the values in a list.
--
-- Examples:
--
-- > -- Zoom into all values of a list (warning: non-strict!):
-- > zoomMeter map :: Meter' [a] -> Meter' a
-- >
-- > -- Zoom into the left component of a tuple:
-- > zoomMeter (\f (x', y) -> let x = f x' in x `seq` (x, y))
-- >     :: Meter' (a, b) -> Meter' a
-- >
-- > -- Zoom into the element indexed by the "foo" key,
-- > -- where M = Data.Map.Strict:
-- > zoomMeter (\f -> M.alter f "foo")
-- >     :: Meter' (M.Map String a) -> Meter' (Maybe a)
-- >
-- > -- Variant of the previous example that always
-- > -- adds the element if it didn't exist before:
-- > zoomMeter (\f -> M.alter (Just . f) "foo")
-- >     :: Meter' (M.Map String a) -> Meter (Maybe a) a

zoomMeter :: ((a -> b) -> s -> t) -> Meter s t -> Meter a b
zoomMeter f meter =
    meter { _mModify = _mModify meter . f }


-- | Variant of 'zoomMeter' that works with van Laarhoven setters as
-- used by libraries like <https://hackage.haskell.org/package/lens lens>:
--
-- > zoomMeterL f = zoomMeter (over f)
--
-- Keep in mind that most predefined lenses are non-strict.  See the
-- tutorial section on zooming to understand why this can be a problem.

zoomMeterL :: ((a -> Identity b) -> s -> Identity t) -> Meter s t -> Meter a b
zoomMeterL f =
    zoomMeter (\g -> runIdentity . f (Identity . g))


{- $tutorial

A progress bar runs concurrently and redraws itself whenever something
changes to inform the impatient user that, yes, your application is
actually doing stuff.  Redraws are throttled to a user-chosen rate in
order not to impact performance in the inconceivable case that changes
come in too frequently.

The 'withProgress_' function adds a progress bar to your application for
the duration of the given action:

> withProgress_
>     :: s
>     -> (s -> String)
>     -> (Meter' s -> IO r)
>     -> IO r

Progress bars maintain mutable state of a user-chosen type @s@ and use a
user-supplied rendering function of type @(s -> String)@ in order to
display the current state whenever your application brings itself to
update it.  The first argument is the initial state, the second argument
is the rendering function.

For example if you would like to display a simple percentage you could
use @s = 'Int'@ and an initial state of @0@.  The rendering function
could turn the plain number into simple text, an ASCII art or any other
single-line entertainment:

> render :: Int -> String
> render x = "Progress: " ++ show x ++ "%"

To change the current state (in this case: the current percentage) you
can use the 'setMeter' function with the @Meter'@ value that your
application receives from 'withProgress_' (simplified type signature):

> setMeter :: Meter' s -> s -> IO ()

Here is a full example in the spirit of the last percent challenging
your patience much worse than the rest:

> import Control.Concurrent
> import Data.Foldable
> import System.Progress
>
> main :: IO ()
> main =
>     withProgress_ 0 render $ \pm -> do
>         for_ [1..99] $ \p -> do
>             threadDelay 20000
>             setMeter pm p
>         threadDelay 3000000
>         setMeter pm 100
>         threadDelay 1000000
>
>     where
>     render :: Int -> String
>     render x = "Progress: " ++ show x ++ "%"

From time to time you might like to perform regular output for
diagnostics, logging or other purposes.  However, you can't just write
to 'stderr' as that would corrupt the progress bar.  Instead you should
use the 'putMsgLn' function (simplified type signature):

> putMsgLn :: Meter' s -> String -> IO ()

You can perform arbitrary actions while temporarily hiding the progress
bar by using the 'meterIO' function, of which 'putMsgLn' is a special
case.

This library fully supports concurrency.  You can use 'setMeter' and
'meterIO' from multiple threads.  The latter will also properly sequence
concurrent actions, so you can safely output diagnostics from multiple
threads.

-}


{- $zooming

A meter of type @('Meter'' s)@ allows you update the current state of
type @s@.  However, especially in highly concurrent applications it can
be useful to give a thread a meter that updates only the part of the
state that is relevant to that thread.  For those applications you can
use the 'zoomMeter' function (simplified type signature):

> zoomMeter :: ((a -> a) -> s -> s) -> Meter' s -> Meter' a

Given a function that can map a function of type @(a -> a)@ over values
of type @s@, this function converts a @('Meter'' s)@ into a @('Meter''
a)@.  This is best illustrated with an example.  The following function
strictly maps over the left component of a tuple:

> {-# LANGUAGE BangPatterns #-}
>
> mapLeft :: (a -> b) -> (a, c) -> (b, c)
> mapLeft f (x', y) = let !x = f x' in (x, y)

You can use this function with 'zoomMeter' to turn a @('Meter'' (a, b))@
into a @('Meter'' a)@:

> zoomMeter mapLeft :: Meter' (a, b) -> Meter' a

This meter can then be used to update only the left component of the
state.  Zooms can be cascaded as well.

If you are using van Laarhoven lenses as defined by the
<https://hackage.haskell.org/package/lens lens library> you can also use
the 'zoomMeterL' function (simplified type signature):

> zoomMeterL :: ASetter' s a -> Meter' s -> Meter' a

Caveat: Unfortunately most if not all of the predefined lenses are
non-strict.  As mentioned earlier the progress bar's rendering loop is
throttled, so a state update may not cause an immediate redraw.  For
that reason the 'setMeter' function updates the state strictly, so that
updates don't cause unevaluated expressions to pile up.  However, it's
only WHNF-strict, so if you do any deep updates using a non-strict
function, they will not be evaluated until the next redraw.  The
@mapLeft@ example above is strict in order to avoid that.

The solution is either to write strict lenses by hand, or to make sure
the state type is fully strict in all its fields on all layers.

-}


{- $concurrent_updates

The rendering loop waits for updates to the current state.  Whenever an
update comes in, it redraws the progress bar and then sleeps for a
user-specified duration (0.1 seconds if you use 'withProgress_').  If
further state updates have been done in the meantime, it redraws itself
and sleeps again, etc.  Otherwise it waits for updates.

Now imagine you need to do two state updates in a row to inform the user
of a certain change, for example you have done one step to completion
and want to start a new phase:

> setMeter statusMeter "Done with foo, now doing bar"
> setMeter percentMeter 0

If the rendering loop is currently in the waiting phase it is very
likely that the first 'setMeter' will immediately wake up the rendering
thread and cause a redraw, after which it goes to its throttle sleep.
Users would then observe a partial state update for a brief amount of
time (the new message, but not the new percentage).  In order to avoid
that you should use the STM variant of 'setMeter' called 'setMeterSTM':

> atomically $ do
>     setMeterSTM statusMeter "Done with foo, now doing bar"
>     setMeterSTM percentMeter 0

This will make sure that the rendering loop never observes a partial
update.

-}


{- $notes

* This library does not do any fancy terminal magic; in particular it
  doesn't check the terminal width, so if the text is too long, the user
  may observe some undesired scrolling.  You may know this effect from
  @curl@.  However, this keeps the implementation simple and portable
  (terminfo is not portable to Windows).

    The author's recommendation is to just ignore this fact.  Even if
    you overdraw the progress bar itself will still work, and it will
    span multiple lines properly.  The scrolling effect is ugly, but
    doesn't severely impact the user experience.

* The default throttle of 0.1 seconds may seem too low, but it really
  isn't.  Keep in mind that the rendering loop does not draw at all,
  unless there are actual updates, so even if your application updates
  very infrequently the default throttle is fine.

-}
