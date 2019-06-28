module Exercises6 where

data TisAnInteger = TisAn Integer
instance Eq TisAnInteger where
    (==) (TisAn int)
         (TisAn int') = int == int'

data TwoIntegers = Two Integer Integer
instance Eq TwoIntegers where
    (==) (Two int1 int2)
         (Two int1' int2') = int1 == int1' && int2 == int2'

data StringOrInt = TisAnInt Int | TisAString String
instance Eq StringOrInt where
    (==) (TisAnInt int) (TisAnInt int') = int == int'
    (==) (TisAString str) (TisAString str') = str == str'
    (==) _ _ = False

data Pair a = Pair a a
instance Eq a => Eq (Pair a) where
    (==) (Pair x y) (Pair x' y') = x==x' && y==y'

data Tuple a b = Tuple a b
instance (Eq a, Eq b) => Eq (Tuple a b) where
    (==) (Tuple x y) (Tuple x' y') = x==x' && y==y'

data Which a = ThisOne a | ThatOne a
instance (Eq a) => Eq (Which a) where
    (==) (ThisOne x) (ThisOne x') = x == x'
    (==) (ThatOne x) (ThatOne x') = x == x'
    (==) _ _ = False

data EitherOr a b = Hello a | Goodbye b
instance (Eq a, Eq b) => Eq (EitherOr a b) where
    (==) (Hello x) (Hello x') = x == x'
    (==) (Goodbye x) (Goodbye x') = x == x'
    (==) _ _ = False

data Person = Person Bool deriving Show
printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

data Mood = Blah | Woot deriving (Show, Eq, Ord)
settleDown :: Mood -> Mood
settleDown x =
    if x == Woot
    then Blah
    else x

type Subject = String
type Verb = String
type Object = String
data Sentence =
    Sentence Subject Verb Object deriving (Eq, Show)
s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"

data Rocks =
    Rocks String deriving (Eq, Show)
data Yeah =
    Yeah Bool deriving (Eq, Show)
data Papu =
    Papu Rocks Yeah deriving (Eq, Show)
