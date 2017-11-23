-- |
-- Copyright:  (c) 2017 Ertugrul Söylemez
-- License:    BSD3
-- Maintainer: Ertugrul Söylemez <esz@posteo.de>

module DevelMain (update) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Data.Foldable
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as Mi
import Data.List (intercalate)
import Rapid
import Rapid.Term
import System.IO
import System.Progress
import Text.Printf


mainWith :: Handle -> IO ()
mainWith h = withProgress prog $ \pm ->
    let thread n =
            (`finally` setMeter tpm Nothing) $ do
                threadDelay (100000*n)
                putMsgLn tpm (printf "Thread %d started." n)
                for_ [0..100] $ \p -> do
                    when (p == 50) $
                        putMsgLn tpm (printf "Thread %d reached half-way point." n)
                    setMeter tpm (Just p)
                    threadDelay (280000 - 40000*n)
                putMsgLn tpm (printf "Thread %d done." n)
                threadDelay 500000


            where
            tpm = zoomMeter (\f -> Mi.alter f n) pm

    in mapConcurrently_ thread [1..6]

    where
    render :: IntMap Int -> String
    render =
        intercalate " | " .
        map (\(tid, p) -> printf "%d: %d" tid p) .
        Mi.assocs

    prog = Progress {
             progressDelay   = 100000,
             progressHandle  = h,
             progressInitial = mempty,
             progressRender  = render
           }


update :: IO ()
update =
    rapid 0 $ \r -> do
        termRef <- createRef r "term-ref" newTermRef
        start r "term" (runTerm (urxvtAt "/home/never/.nix-profile/bin/urxvt") termRef)
        restart r "app" . stats termRef . terminal termRef $ mainWith
