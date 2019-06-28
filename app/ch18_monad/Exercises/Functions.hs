module Functions where

j :: Monad m => m (m a) -> m a
j mma = mma >>= id

-- l1 :: Monad m => (a -> b) -> m a -> m b
-- l1 = (<$>)

-- l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
-- l2 f ma mb = f <$> ma <*> mb

-- a :: Monad m => m a -> m (a -> b) -> m b
-- a = flip (<*>)

-- meh :: Monad m => [a] -> (a -> m b) -> m [b]
-- meh as f =
--     where
--         mbs = f <$> as
--         mbsToMlb [] bs = return bs
--         mbsToMlb (mb : mbs) bs = mb >>= (\b -> return (b : mbsToMlb mbs bs))

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = (<$>)

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f ma mb = f <$> ma >>= (<$> mb)

a :: Monad m => m a -> m (a -> b) -> m b
a ma mf = mf >>= (<$> ma)

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (a':as) f = do
    b <- f a'
    bs <- meh as f
    return (b:bs)

flipType :: (Monad m) => [m a] -> m [a]
flipType mas = meh mas id
