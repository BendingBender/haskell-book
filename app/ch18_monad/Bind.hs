module Bind where

import Control.Monad (join)

-- join :: Monad m => m (m a) -> m a
-- fmap :: (a -> b) -> f a -> f b

bind :: Monad m => m a -> (a -> m b) -> m b
bind ma f =
    join $ fmap f ma