{-# LANGUAGE FlexibleInstances #-}

module Ch16_functor.Exercises.FunctorInstances where

data Quant a b = Finance | Desk a | Bloor b deriving (Eq, Show)
instance Functor (Quant a) where
    fmap f (Bloor b) = Bloor $ f b
    fmap _ Finance = Finance
    fmap _ (Desk a) = Desk a

data K a b = K a
instance Functor (K a) where
    fmap _ (K a) = K a

newtype C a b = C a
newtype Flip f a b = Flip (f b a) deriving (Eq, Show)
instance Functor (Flip C a) where
    fmap f (Flip (C a)) = Flip (C $ f a)

data EvilGoateeConst a b = GoatyConst b
instance Functor (EvilGoateeConst a) where
    fmap f (GoatyConst b) = GoatyConst $ f b

data LiftItOut f a = LiftItOut (f a)
instance Functor f => Functor (LiftItOut f) where
    fmap f (LiftItOut fa) = LiftItOut $ fmap f fa

data Parappa f g a = DaWrappa (f a) (g a)
instance (Functor f, Functor g) => Functor (Parappa f g) where
    fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)

data IgnoreOne f g a b = IgnoringSomething (f a) (g b)
instance Functor g => Functor (IgnoreOne f g a) where
    fmap f (IgnoringSomething fa gb) = IgnoringSomething fa $ fmap f gb

data Notorious g o a t = Notorious (g o) (g a) (g t)
instance Functor g => Functor (Notorious g o a) where
    fmap f (Notorious go ga gt) = Notorious go ga $ fmap f gt

data List a = Nil | Cons a (List a)
instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons a as) = Cons (f a) (fmap f as)

data GoatLord a =
    NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
instance Functor GoatLord where
    fmap _ NoGoat = NoGoat
    fmap f (OneGoat a) = OneGoat $ f a
    fmap f (MoreGoats gs gs' gs'') = MoreGoats (fmap f gs) (fmap f gs') (fmap f gs'')

data TalkToMe a = Halt | Print String a | Read (String -> a)
instance Functor TalkToMe where
    fmap _ Halt = Halt
    fmap f (Print s a) = Print s $ f a
    fmap f (Read sToA) = Read $ fmap f sToA
instance Show a => Show (TalkToMe a) where
    show Halt = "Halt"
    show (Print s a) = "Print " ++ s ++ " " ++ show a
    show (Read _) = "Read (String -> a)"