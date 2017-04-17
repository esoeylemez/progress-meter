-- |
-- Copyright:  (c) 2017 Ertugrul Söylemez
-- License:    BSD3
-- Maintainer: Ertugrul Söylemez <esz@posteo.de>

module DevelMain (update) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Control.Monad.Codensity
import Control.Monad.IO.Class
import Data.Foldable
import Rapid
import Rapid.Term
import System.IO
import System.ProgressMeter
import Text.Printf


testApp :: Handle -> IO ()
testApp h =
    lowerCodensity $ do
        pm <- Codensity (hWithProgress 100000 h)

        Codensity $ withAsync $ do
            m <- appendMeter pm
            for_ [0,2..50] $ \x -> do
                setMeter m ('(' : show x ++ ")")
                threadDelay 200000

        Codensity $ withAsync . withAppendMeter pm $ \m -> do
            for_ [1,3..1000] $ \x -> do
                setMeter m ('(' : show x ++ ")")
                threadDelay 10000

        Codensity $ withAsync $ do
            for_ [0..] $ \x -> do
                putMsgLn pm ("M: " ++ show x)
                threadDelay 1000000

        liftIO (threadDelay 10000000)


mainWith :: Handle -> IO ()
mainWith h =
    hWithProgress 500000 h $ \prog ->
        let thread n = do
                threadDelay (100000*n)
                withAppendMeter prog $ \meter -> do
                    putMsgLn prog (printf "Thread %d started." n)
                    for_ [0..100 :: Int] $ \p -> do
                        when (p == 50) $
                            putMsgLn prog (printf "Thread %d reached half-way point." n)
                        setMeter meter (printf "T%d: %d%%" n p)
                        threadDelay (280000 - 40000*n)
                    putMsgLn prog (printf "Thread %d done." n)
                    threadDelay 500000
        in mapConcurrently_ thread [1..6]


update :: IO ()
update =
    rapid 0 $ \r -> do
        termRef <- createRef r "term-ref" newTermRef
        start r "term" (runTerm (urxvtAt "/home/never/.nix-profile/bin/urxvt") termRef)
        restart r "app" . stats termRef . terminal termRef $ mainWith
