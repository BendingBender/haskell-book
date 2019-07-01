module Ch20_foldable.FilterFn where

-- foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m

filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF f =
    let
        toStructure a =
            if f a then pure a else mempty
    in
        foldMap toStructure