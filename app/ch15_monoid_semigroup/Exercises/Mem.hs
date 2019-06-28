module Mem where

import Test.QuickCheck
import Data.Monoid

newtype Mem s a =
    Mem {
        runMem :: s -> (a,s)
    }

instance Show (Mem s a) where
    show _ = "Mem {runMem = s -> (a,s)}"

instance Monoid a => Monoid (Mem s a) where
    mempty = Mem (\s -> (mempty, s))
    mappend (Mem m) (Mem m') = Mem comb
        where
            comb s = (a', s')
                where
                    as = m s
                    as' = m' $ snd as
                    a' = fst as <> fst as'
                    s' = snd as'

instance (CoArbitrary s, Arbitrary s, CoArbitrary a, Arbitrary a)
    => Arbitrary (Mem s a) where
    arbitrary = do
        f <- arbitrary
        return $ Mem f

f' :: Mem Int String
f' = Mem $ \s -> ("hi", s + 1)

prop_MemSemigroupAssoc :: (Eq s, Eq a, Monoid a) =>
    Mem s a -> Mem s a -> Mem s a -> s -> Bool
prop_MemSemigroupAssoc m m' m'' s =
    (runMem (m <> (m' <> m'')) $ s) == (runMem ((m <> m') <> m'') $ s)

prop_MemMonoidLeftIdentity :: (Eq s, Eq a, Monoid a) => Mem s a -> s -> Bool
prop_MemMonoidLeftIdentity m s = (runMem (mempty <> m) $ s) == (runMem m $ s)

prop_MemMonoidRightIdentity :: (Eq s, Eq a, Monoid a) => Mem s a -> s -> Bool
prop_MemMonoidRightIdentity m s = (runMem (m <> mempty) $ s) == (runMem m $ s)

type MIS = Mem Int String
type MemAssoc = MIS -> MIS -> MIS -> Int -> Bool

main :: IO ()
main = do
    quickCheck (prop_MemSemigroupAssoc :: MemAssoc)
    quickCheck (prop_MemMonoidLeftIdentity :: MIS -> Int -> Bool)
    quickCheck (prop_MemMonoidRightIdentity :: MIS -> Int -> Bool)
    let rmzero = runMem mempty 0
        rmleft = runMem (f' <> mempty) 0
        rmright = runMem (mempty <> f') 0
    print $ rmleft
    print $ rmright
    print $ (rmzero :: (String, Int))
    print $ rmleft == runMem f' 0
    print $ rmright == runMem f' 0