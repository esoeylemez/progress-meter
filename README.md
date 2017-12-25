Progress-Meter
==============

This library can be used to display a progress bar or other live
diagnostics for your application.  It supports partial updates from
multiple threads without interfering with each other, and it has the
correct behaviour when printing diagnostics that are not part of the
progress bar and should just scroll by.

The `System.Progress` module contains a tutorial.


Quickstart
----------

Use the `withProgress_` function to create a progress bar for the
duration of an action:

``` haskell
withProgress_
    :: s                   -- ^ Initial state value
    -> (s -> String)       -- ^ State renderer
    -> (Meter' s -> IO r)  -- ^ Action with a progress bar
    -> IO r
```

You need to choose a state type and a renderer for the state that is
used to display it in the terminal.  Then you can use `setMeter` or
`modifyMeter` to update the state value, which triggers a redraw of the
progress bar:

``` haskell
setMeter    :: Meter' s -> s -> IO ()
modifyMeter :: Meter' s -> (s -> s) -> IO ()
```

To perform regular output (logging or other diagnostics that should
scroll by) use the `putMsgLn` function:

``` haskell
putMsgLn :: Meter' s -> String -> IO ()
```

You can use the `zoomMeter` function to give individual threads a meter
that only changes part of the state:

``` haskell
zoomMeter :: ((a -> a) -> s -> s) -> Meter' s -> Meter' a
```


Simple example
--------------

Taken from the tutorial the following uses a progress bar with state of
type `Int` and renders it to a simple percentage display:

``` haskell
import Control.Concurrent
import Data.Foldable
import System.Progress

main :: IO ()
main =
    withProgress_ 0 render $ \pm -> do
        for_ [1..99] $ \p -> do
            threadDelay 20000
            setMeter pm p
        threadDelay 3000000
        setMeter pm 100
        threadDelay 1000000

    where
    render :: Int -> String
    render x = "Progress: " ++ show x ++ "%"
```


Complex example
---------------

The following is an example of how you can use `zoomMeter` to display
multiple progress indicators for concurrent threads and update each of
them independently.  The program takes a bunch of command line arguments
and launches a separate worker thread for each.  The overall progress
state is represented by a value of type `(Map FilePath Int)` that keeps
track of the percentage of each individual worker:

``` haskell
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Data.Foldable
import Data.List (intercalate)
import qualified Data.Map.Strict as M
import System.Environment
import System.Progress
import System.Random


-- Worker threads take a meter that points to an individual percentage.
-- The percentage is 'Maybe'-wrapped, because 'Nothing' represents
-- absence of the percentage.

thread :: Meter' (Maybe Int) -> FilePath -> IO ()
thread pm fp =
    -- Add the percentage for this file path at the beginning and make
    -- sure it disappears at the end
    bracket_ (setMeter pm (Just 0)) (setMeter pm Nothing) $ do

        -- Simulate real work by choosing a random delay for each thread
        delay <- randomRIO (100000, 200000)

        putMsgLn pm ("Started work on " ++ fp)

        -- Do the actual "work"
        for_ [1..99] $ \p -> do
            when (p == 50)
                 (putMsgLn pm (fp ++ " is half-way done"))
            setMeter pm (Just p)
            threadDelay delay

        putMsgLn pm ("Done with " ++ fp)


-- This function turns a meter that points to a 'Map k a' into a meter
-- that points to a specific key.  'Nothing' represents lack of that
-- particular key in the map.

zoomKey :: (Ord k) => k -> Meter' (M.Map k a) -> Meter' (Maybe a)
zoomKey k = zoomMeter (\f -> M.alter f k)


main :: IO ()
main =
    -- The initial progress state is the empty map
    withProgress_ M.empty render $ \pm ->
        getArgs >>=

        -- The 'zoomMeter' function is used here to construct a meter
        -- that points to an individual key of the map:
        mapConcurrently_ (\fp -> thread (zoomKey fp pm) fp)

    where
    -- The renderer displays something like this:
    -- a: 21% | b: 50% | c: 8%
    render :: M.Map FilePath Int -> String
    render =
        intercalate " | " .
        map (\(fp, p) -> fp ++ ": " ++ show p ++ "%") .
        M.assocs
```
