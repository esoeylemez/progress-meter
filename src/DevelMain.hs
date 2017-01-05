-- |
-- Copyright:  (c) 2016 Ertugrul Söylemez
-- License:    BSD3
-- Maintainer: Ertugrul Söylemez <esz@posteo.de>

module DevelMain (update) where

import Rapid


update :: IO ()
update =
    rapid 0 $ \r ->
        pure ()
