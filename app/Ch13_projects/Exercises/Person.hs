module Ch13_projects.Exercises.Person where

import Text.Read (readMaybe)

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid =
    NameEmpty
    | AgeTooLow
    | PersonInvalidUnknown String
    deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
    | name /= "" && age > 0 = Right $ Person name age
    | name == "" = Left NameEmpty
    | not (age > 0) = Left AgeTooLow
    | otherwise =
        Left $ PersonInvalidUnknown $
            "Name was: " ++ show name ++
            " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
    putStrLn "Enter name:"
    name <- getLine
    putStrLn "Enter age:"
    ageStr <- getLine
    case readMaybe ageStr of
        Just age -> do
            case mkPerson name age of
                Right person -> do
                    putStrLn $ "Yay! Successfully got a person: "
                        ++ show person
                Left reason -> do
                    putStrLn $ "Could not make a person: "
                        ++ show reason
        Nothing -> putStrLn $ "'" ++ ageStr ++ "' is not a valid number!"